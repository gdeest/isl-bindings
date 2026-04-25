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
  , defaultMappingV2
  , declListNames
  , declListBoundsM
  , declBoundsV2
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
import qualified Alpha.Core as V2
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

-- | V2 'CFunctionMapping' from a Core 'V2.System'.  Same defaults as
-- 'defaultMapping' (inputs/outputs caller-allocated, locals locally
-- managed); names come from 'V2.SomeVarDecl' instead of the surface
-- 'DeclList'.
defaultMappingV2
  :: forall sys a.
     String
  -> V2.System sys a
  -> CFunctionMapping
defaultMappingV2 name sys =
  let inputNames  = map someVarName (V2.sysInputs  sys)
      outputNames = map someVarName (V2.sysOutputs sys)
      localNames  = map someVarName (V2.sysLocals  sys)
      passing = Map.fromList $
        [ (n, CallerAllocated) | n <- inputNames ]
        ++ [ (n, CallerAllocated) | n <- outputNames ]
        ++ [ (n, LocallyManaged) | n <- localNames ]
  in CFunctionMapping { cfName = name, cfArgPassing = passing }

someVarName :: V2.SomeVarDecl sys -> String
someVarName (V2.SomeVarDecl _ vd) = V2.vdName vd

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

-- | V2 bounds extractor.  Mirrors 'declListBoundsM' for a list of
-- 'V2.SomeVarDecl': pattern-match on conjunctions first, fall back to
-- ISL @dim_max@ on the materialised 'NamedSet' when needed.
declBoundsV2
  :: forall sys.
     [String] -> [V2.SomeVarDecl sys]
  -> IslT IO (Ur (Either (String, Int, BoundErr) (Map String [String])))
declBoundsV2 _params [] = Isl.pure (Ur (Right Map.empty))
declBoundsV2 params (V2.SomeVarDecl _ vd : rest) = Isl.do
  let name  = V2.vdName vd
      nDims = V2.vdDims vd
      ns    = Named.the (V2.vdDom vd)
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
  Ur restRes <- declBoundsV2 params rest
  Isl.pure (Ur (do
    here <- hereRes
    rm   <- restRes
    Right (Map.insert name here rm)))
