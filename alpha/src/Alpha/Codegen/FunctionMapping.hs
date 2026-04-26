{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | C function signature and buffer management for generated code.
module Alpha.Codegen.FunctionMapping
  ( ArgPassing(..)
  , CFunctionMapping(..)
  , defaultMapping
  , declListNames
  , declListBoundsM
  , declBounds
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal, symbolVal)

import Isl.Typed.Constraints (Conjunction(..), NamedSet(..))
import Isl.Typed.Params (KnownSymbols)
import Isl.TypeLevel.Reflection (reflectDomConstraints)
import Isl.Monad (IslT, Ur(..))
import qualified Isl.Linear as Isl
import Alpha.Surface.Core
  ( DeclName, DeclDims, DeclDomTag
  , DeclList(..), Decl(..), Decls(..), System, pattern System )
import qualified Alpha.Core as Core
import qualified Alpha.Core.Named as Named
import Alpha.Codegen.ExprRender
  ( extractOneBound, extractBoundsISLM, extractBoundsFromSetM, BoundErr )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Types
-- ═══════════════════════════════════════════════════════════════════════

data ArgPassing
  = CallerAllocated
  | LocallyManaged
  deriving (Show, Eq, Ord)

data CFunctionMapping = CFunctionMapping
  { cfName       :: !String
  , cfArgPassing :: !(Map String ArgPassing)
  } deriving (Show)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Default mapping
-- ═══════════════════════════════════════════════════════════════════════

-- | Default: inputs and outputs are 'CallerAllocated',
-- locals are 'LocallyManaged'.
defaultMapping
  :: String
  -> System ps pctx inputs outputs locals
  -> CFunctionMapping
defaultMapping name (System decls _eqs) =
  let inputNames  = declListNames (dInputs decls)
      outputNames = declListNames (dOutputs decls)
      localNames  = declListNames (dLocals decls)
      passing = Map.fromList $
        [ (n, CallerAllocated) | n <- inputNames ]
        ++ [ (n, CallerAllocated) | n <- outputNames ]
        ++ [ (n, LocallyManaged) | n <- localNames ]
  in CFunctionMapping { cfName = name, cfArgPassing = passing }

declListNames :: forall ps decls. DeclList ps decls -> [String]
declListNames Nil = []
declListNames ((MkDecl :: Decl ps d) :> rest) =
  symbolVal (Proxy @(DeclName d)) : declListNames rest

-- | Compute per-variable exclusive upper bounds for each declaration
-- in a 'DeclList'.  Uses the fast structural pattern matcher first and
-- falls back to ISL @dim_max@ (via 'extractBoundsISLM') when needed.
--
-- Runs in 'IslT' so ISL failures surface as @Left (name, dim, err)@
-- rather than sentinel strings or 'unsafePerformIO' escapes.
declListBoundsM
  :: forall ps decls. KnownSymbols ps => [String] -> DeclList ps decls
  -> IslT IO (Ur (Either (String, Int, BoundErr) (Map String [String])))
declListBoundsM _params Nil = Isl.pure (Ur (Right Map.empty))
declListBoundsM params ((MkDecl :: Decl ps d) :> rest) = Isl.do
  let name  = symbolVal (Proxy @(DeclName d))
      nDims = fromIntegral (natVal (Proxy @(DeclDims d))) :: Int
      cs    = reflectDomConstraints @ps @(DeclDims d) @(DeclDomTag d)
      conjs = [Conjunction cs]
      patternBounds = [ extractOneBound params conjs dim | dim <- [0..nDims-1] ]
  Ur hereRes <-
    if any isNothing patternBounds
      then Isl.do
        Ur r <- extractBoundsISLM @ps @(DeclDims d) @(DeclDomTag d) nDims
        Isl.pure (Ur (case r of
          Left (d', err) -> Left (name, d', err)
          Right bs       -> Right bs))
      else Isl.pure (Ur (Right [ b | Just b <- patternBounds ]))
  Ur restRes <- declListBoundsM params rest
  Isl.pure (Ur (do
    here <- hereRes
    rm   <- restRes
    Right (Map.insert name here rm)))

-- | Bounds extractor over a list of 'Core.SomeVarDecl': pattern-match
-- on conjunctions first, fall back to ISL @dim_max@ on the
-- materialised 'NamedSet' when needed.
declBounds
  :: forall sys.
     [String] -> [Core.SomeVarDecl sys]
  -> IslT IO (Ur (Either (String, Int, BoundErr) (Map String [String])))
declBounds _params [] = Isl.pure (Ur (Right Map.empty))
declBounds params (Core.SomeVarDecl _ vd : rest) = Isl.do
  let name  = Core.vdName vd
      nDims = Core.vdDims vd
      ns    = Named.the (Core.vdDom vd)
      conjs = nsConjs ns
      patternBounds = [ extractOneBound params conjs dim | dim <- [0..nDims-1] ]
  Ur hereRes <-
    if any isNothing patternBounds
      then Isl.do
        Ur r <- extractBoundsFromSetM ns nDims
        Isl.pure (Ur (case r of
          Left (d', err) -> Left (name, d', err)
          Right bs       -> Right bs))
      else Isl.pure (Ur (Right [ b | Just b <- patternBounds ]))
  Ur restRes <- declBounds params rest
  Isl.pure (Ur (do
    here <- hereRes
    rm   <- restRes
    Right (Map.insert name here rm)))
