{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Phantom-indexed pure Haskell representations of ISL objects.
-- These types carry parameter names at the type level (@ps@) and
-- dimension counts (@n@), ensuring you cannot accidentally mix
-- representations of different parameter spaces or dimensionality.
--
-- Use 'decomposeBS', 'decomposeSet', etc. to convert from ISL objects
-- to these pure types, and 'mkBasicSet', 'toBasicSet' etc. to go back.
module Isl.HighLevel.Pure
  ( -- * Set-like pure representations
    PConjunction(..)
  , PDisjunction(..)
    -- * Map-like pure representations
  , PMapConjunction(..)
  , PMapDisjunction(..)
    -- * Existential wrappers (for union types with unknown dimensionality)
  , SomeDisjunction(..)
  , SomeMapDisjunction(..)
  ) where

import GHC.TypeLits (Nat, KnownNat, Symbol)

import Isl.HighLevel.Constraints (Conjunction, SetIx, MapIx)
import Isl.HighLevel.Params (KnownSymbols)

-- | A single convex polyhedron (conjunction of constraints) with
-- parameter names @ps@ and @n@ set dimensions. Pure Haskell — no ISL pointers.
newtype PConjunction (ps :: [Symbol]) (n :: Nat) = PConjunction (Conjunction SetIx)

deriving instance Show (PConjunction ps n)
deriving instance Eq (PConjunction ps n)

-- | A union of convex polyhedra (disjunction of conjunctions) with
-- parameter names @ps@ and @n@ set dimensions.
-- Represents the same thing as an ISL 'Set'.
newtype PDisjunction (ps :: [Symbol]) (n :: Nat) = PDisjunction [PConjunction ps n]

deriving instance Show (PDisjunction ps n)
deriving instance Eq (PDisjunction ps n)

-- | A single convex polyhedron in map space with parameter names @ps@,
-- @nIn@ input dimensions and @nOut@ output dimensions.
-- Constraints use 'MapIx' to distinguish input ('InDim'), output ('OutDim'),
-- and parameter ('MapParam') variables.
newtype PMapConjunction (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = PMapConjunction (Conjunction MapIx)

deriving instance Show (PMapConjunction ps nIn nOut)
deriving instance Eq (PMapConjunction ps nIn nOut)

-- | A union of map polyhedra with parameter names @ps@,
-- @nIn@ input and @nOut@ output dimensions.
-- Represents the same thing as an ISL 'Map'.
newtype PMapDisjunction (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = PMapDisjunction [PMapConjunction ps nIn nOut]

deriving instance Show (PMapDisjunction ps nIn nOut)
deriving instance Eq (PMapDisjunction ps nIn nOut)

-- | Existentially-quantified disjunction. Used when deconstructing
-- union types where the dimension count and parameters are not statically known.
data SomeDisjunction = forall ps n. (KnownSymbols ps, KnownNat n) => SomeDisjunction (PDisjunction ps n)

instance Show SomeDisjunction where
  show (SomeDisjunction d) = show d

-- | Existentially-quantified map disjunction.
data SomeMapDisjunction = forall ps ni no. (KnownSymbols ps, KnownNat ni, KnownNat no) => SomeMapDisjunction (PMapDisjunction ps ni no)

instance Show SomeMapDisjunction where
  show (SomeMapDisjunction d) = show d
