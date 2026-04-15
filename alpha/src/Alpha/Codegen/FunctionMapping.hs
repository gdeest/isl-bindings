{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | C function signature and buffer management for generated code.
module Alpha.Codegen.FunctionMapping
  ( ArgPassing(..)
  , CFunctionMapping(..)
  , defaultMapping
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Alpha.Core (VarDecl(..), DeclName, DeclList(..), Decl(..), Decls(..), System, pattern System)


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
  -> System ps inputs outputs locals
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
