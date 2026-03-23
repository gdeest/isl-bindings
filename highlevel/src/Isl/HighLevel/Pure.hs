{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Phantom-indexed pure Haskell representations of ISL objects.
-- These types carry dimension counts at the type level, ensuring
-- you cannot accidentally mix representations of different dimensionality.
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

import GHC.TypeLits (Nat, KnownNat)

import Isl.HighLevel.Constraints (Conjunction, MapDim)

-- | A single convex polyhedron (conjunction of constraints) with @n@
-- set dimensions. Pure Haskell — no ISL pointers.
newtype PConjunction (n :: Nat) = PConjunction (Conjunction Integer)

deriving instance Show (PConjunction n)
deriving instance Eq (PConjunction n)

-- | A union of convex polyhedra (disjunction of conjunctions) with @n@
-- set dimensions. Represents the same thing as an ISL 'Set'.
newtype PDisjunction (n :: Nat) = PDisjunction [PConjunction n]

deriving instance Show (PDisjunction n)
deriving instance Eq (PDisjunction n)

-- | A single convex polyhedron in map space with @nIn@ input dimensions
-- and @nOut@ output dimensions. Constraints use 'MapDim' to distinguish
-- input ('InDim') from output ('OutDim') variables.
newtype PMapConjunction (nIn :: Nat) (nOut :: Nat) = PMapConjunction (Conjunction MapDim)

deriving instance Show (PMapConjunction nIn nOut)
deriving instance Eq (PMapConjunction nIn nOut)

-- | A union of map polyhedra with @nIn@ input and @nOut@ output dimensions.
-- Represents the same thing as an ISL 'Map'.
newtype PMapDisjunction (nIn :: Nat) (nOut :: Nat) = PMapDisjunction [PMapConjunction nIn nOut]

deriving instance Show (PMapDisjunction nIn nOut)
deriving instance Eq (PMapDisjunction nIn nOut)

-- | Existentially-quantified disjunction. Used when deconstructing
-- union types where the dimension count is not statically known.
data SomeDisjunction = forall n. KnownNat n => SomeDisjunction (PDisjunction n)

instance Show SomeDisjunction where
  show (SomeDisjunction d) = show d

-- | Existentially-quantified map disjunction.
data SomeMapDisjunction = forall ni no. (KnownNat ni, KnownNat no) => SomeMapDisjunction (PMapDisjunction ni no)

instance Show SomeMapDisjunction where
  show (SomeMapDisjunction d) = show d
