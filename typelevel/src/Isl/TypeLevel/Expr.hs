{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

-- | Type-level affine expressions over polyhedral dimensions and parameters.
--
-- These mirror the value-level 'Isl.HighLevel.Constraints.Expr' but live
-- entirely at the type level, so GHC (and our plugin) can reason about them
-- during compilation.
--
-- GHC has no type-level 'Integer', so we introduce 'Z' to represent signed
-- integers at the kind level.
module Isl.TypeLevel.Expr
  ( -- * Type-level signed integers
    Z(..)
    -- * Type-level affine expressions
  , TExpr(..)
    -- * Sugar: type-level operators
  , type (+.), type (-.), type (*.)
    -- * Validation type families
  , Assert, NatLT, Elem
  , CheckDim, CheckParam, ValidExpr
  ) where

import Data.Kind (Constraint)
import GHC.TypeLits (Nat, Symbol, CmpNat, CmpSymbol, ErrorMessage(..), TypeError)

-- | Signed integers at the type/kind level.
--
-- @'Pos' 3@ represents +3, @'Neg' 2@ represents −2, @'Pos' 0@ is zero.
data Z = Pos Nat | Neg Nat

-- | Type-level affine expressions.
--
-- Variables are referenced either by dimension index ('TDim') or by
-- parameter name ('TParam').  Constants use 'TConst' with a 'Z'.
--
-- This mirrors 'Isl.HighLevel.Constraints.Expr' but promoted to kinds.
data TExpr
  = TDim Nat           -- ^ Dimension variable by positional index
  | TParam Symbol      -- ^ Parameter variable by name (matches ISL naming)
  | TConst Z           -- ^ Integer constant
  | TAdd TExpr TExpr   -- ^ Addition of two expressions
  | TMul Z TExpr       -- ^ Scalar multiplication

-- | @a +. b@  =  @'TAdd' a b@
type family (a :: TExpr) +. (b :: TExpr) :: TExpr where
  a +. b = 'TAdd a b

-- | @a -. b@  =  @'TAdd' a ('TMul' ('Neg' 1) b)@
type family (a :: TExpr) -. (b :: TExpr) :: TExpr where
  a -. b = 'TAdd a ('TMul ('Neg 1) b)

-- | @k *. a@  =  @'TMul' k a@
type family (k :: Z) *. (a :: TExpr) :: TExpr where
  k *. a = 'TMul k a

-- * Validation type families

-- | Assert a boolean condition at the type level, with a custom error message.
type family Assert (b :: Bool) (msg :: ErrorMessage) :: Constraint where
  Assert 'True  _   = ()
  Assert 'False msg = TypeError msg

-- | Type-level @a < b@ for 'Nat', returning 'Bool'.
type family NatLT (a :: Nat) (b :: Nat) :: Bool where
  NatLT a b = OrdLT (CmpNat a b)

type family OrdLT (o :: Ordering) :: Bool where
  OrdLT 'LT = 'True
  OrdLT _   = 'False

-- | Type-level membership test: is symbol @s@ in the list @ps@?
type family Elem (s :: Symbol) (ps :: [Symbol]) :: Bool where
  Elem _ '[]       = 'False
  Elem s (p ': ps) = ElemCase (CmpSymbol s p) s ps

type family ElemCase (o :: Ordering) (s :: Symbol) (ps :: [Symbol]) :: Bool where
  ElemCase 'EQ _ _  = 'True
  ElemCase _   s ps = Elem s ps

-- | Check that dimension index @d@ is in bounds for an @n@-dimensional space.
type family CheckDim (d :: Nat) (n :: Nat) :: Constraint where
  CheckDim d n = Assert (NatLT d n)
    ('Text "Dimension index " ':<>: 'ShowType d
     ':<>: 'Text " out of bounds for "
     ':<>: 'ShowType n ':<>: 'Text "-dimensional space")

-- | Check that parameter name @s@ appears in the parameter list @ps@.
type family CheckParam (s :: Symbol) (ps :: [Symbol]) :: Constraint where
  CheckParam s ps = Assert (Elem s ps)
    ('Text "Unknown parameter " ':<>: 'ShowType s
     ':<>: 'Text " not in parameter list " ':<>: 'ShowType ps)

-- | Validate all dimension/parameter references in a 'TExpr' against
-- a parameter list @ps@ and dimension count @n@.
type family ValidExpr (ps :: [Symbol]) (n :: Nat) (e :: TExpr) :: Constraint where
  ValidExpr _  n ('TDim d)   = CheckDim d n
  ValidExpr ps _ ('TParam s) = CheckParam s ps
  ValidExpr _  _ ('TConst _) = ()
  ValidExpr ps n ('TAdd a b) = (ValidExpr ps n a, ValidExpr ps n b)
  ValidExpr ps n ('TMul _ a) = ValidExpr ps n a
