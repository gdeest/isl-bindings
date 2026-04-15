{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Memory allocation for Alpha systems.
--
-- An allocation assigns a storage strategy to each defined variable
-- (output or local).  'FullStorage' materialises the full iteration
-- domain; 'Contracted' applies a modular reduction map so that a
-- time-contracted buffer can be reused.
--
-- = Typed surface
--
-- @
-- alloc = allocating $ do
--   alloc \@\"C\" \@MatmulDecls FullStorage
--   alloc \@\"u\" \@Heat3DDecls (Contracted (contractedModular 2))
-- @
module Alpha.Allocation
  ( -- * Types
    EqStorage(..)
  , Allocation(..)
    -- * Builder
  , AllocationBuilder
  , allocating
    -- * Typed combinator
  , alloc
    -- * Untyped combinator
  , allocate
    -- * Defaults
  , defaultAllocation
    -- * Re-exports
  , C.StorageMap(..)
  ) where

import Control.Monad.State.Strict (State, modify', execState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Alpha.Polyhedral.Contraction as C
import Isl.Typed.Constraints (NamedSet(..))

import Alpha.Core (VarDecl(..), Lookup)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Types
-- ═══════════════════════════════════════════════════════════════════════

-- | Storage strategy for one equation.
data EqStorage
  = FullStorage
    -- ^ Array matches iteration domain exactly.
  | Contracted !C.StorageMap
    -- ^ Affine map from virtual dims to physical buffer
    -- (e.g., time modular contraction).
  deriving (Show)

-- | Storage for an entire system — one entry per defined variable.
newtype Allocation = Allocation { allocEntries :: Map String EqStorage }
  deriving (Show)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Builder
-- ═══════════════════════════════════════════════════════════════════════

-- | Monadic allocation builder.
type AllocationBuilder a = State (Map String EqStorage) a

-- | Build an 'Allocation' from a builder.
allocating :: AllocationBuilder () -> Allocation
allocating = Allocation . flip execState Map.empty


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Typed combinator
-- ═══════════════════════════════════════════════════════════════════════

-- | Assign storage to a named variable.  Unknown variable names are
-- a compile error via 'Lookup'.
--
-- @
-- alloc \@\"C\" \@MatmulDecls FullStorage
-- @
alloc
  :: forall (name :: Symbol) {ps} (decls :: [VarDecl ps]).
     ( Lookup name decls ~ Lookup name decls  -- forces Lookup to reduce, erroring on unknown names
     , KnownSymbol name
     )
  => EqStorage -> AllocationBuilder ()
alloc storage = allocate (symbolVal (Proxy @name)) storage


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Untyped combinator
-- ═══════════════════════════════════════════════════════════════════════

-- | Add storage for one equation (untyped).
allocate :: String -> EqStorage -> AllocationBuilder ()
allocate name storage = modify' (Map.insert name storage)


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Defaults
-- ═══════════════════════════════════════════════════════════════════════

-- | Default allocation: 'FullStorage' for every equation domain.
defaultAllocation :: [NamedSet] -> Allocation
defaultAllocation domains = Allocation $ Map.fromList
  [ (name, FullStorage)
  | NamedSet { nsName = Just name } <- domains
  ]
