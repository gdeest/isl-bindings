{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Phantom-indexed pure Haskell representations of polyhedral objects.
-- These types carry parameter names (@ps@) and dimension counts (@n@)
-- at the type level — needed by the type-level DSL and singletons.
--
-- For value-level representations (NamedSet, NamedMap), see
-- "Isl.Typed.Constraints".
module Isl.Typed.Pure
  ( -- * Set-like pure representations
    PConjunction(..)
  , PDisjunction(..)
    -- * Map-like pure representations
  , PMapConjunction(..)
  , PMapDisjunction(..)
    -- * Multi-aff / piecewise pure representations
  , PMultiAff(..)
  , PPwAff(..)
  , PPwMultiAff(..)
  ) where

import Control.DeepSeq (NFData(..))
import GHC.TypeLits (Nat, Symbol)

import Isl.Typed.Constraints (Conjunction, Expr, SetIx, MapIx)

-- =========================================================================
-- Phantom-indexed pure representations
-- =========================================================================

newtype PConjunction (ps :: [Symbol]) (n :: Nat) = PConjunction (Conjunction SetIx)

deriving instance Show (PConjunction ps n)
deriving instance Eq (PConjunction ps n)

instance NFData (PConjunction ps n) where
  rnf (PConjunction c) = rnf c

newtype PDisjunction (ps :: [Symbol]) (n :: Nat) = PDisjunction [PConjunction ps n]

deriving instance Show (PDisjunction ps n)
deriving instance Eq (PDisjunction ps n)

instance NFData (PDisjunction ps n) where
  rnf (PDisjunction cs) = rnf cs

newtype PMapConjunction (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = PMapConjunction (Conjunction MapIx)

deriving instance Show (PMapConjunction ps nIn nOut)
deriving instance Eq (PMapConjunction ps nIn nOut)

newtype PMapDisjunction (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = PMapDisjunction [PMapConjunction ps nIn nOut]

deriving instance Show (PMapDisjunction ps nIn nOut)
deriving instance Eq (PMapDisjunction ps nIn nOut)

newtype PMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = PMultiAff [Expr SetIx]

deriving instance Show (PMultiAff ps ni no)
deriving instance Eq (PMultiAff ps ni no)

newtype PPwAff (ps :: [Symbol]) (n :: Nat) = PPwAff [(Conjunction SetIx, Expr SetIx)]

deriving instance Show (PPwAff ps n)
deriving instance Eq (PPwAff ps n)

newtype PPwMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = PPwMultiAff [(Conjunction SetIx, [Expr SetIx])]

deriving instance Show (PPwMultiAff ps ni no)
deriving instance Eq (PPwMultiAff ps ni no)
