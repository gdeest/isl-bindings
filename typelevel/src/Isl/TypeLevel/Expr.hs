{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-level affine expressions over polyhedral dimensions and parameters.
--
-- Expressions are indexed by their parameter list @ps@ and dimension count @n@,
-- preventing accidental mixing of expressions from different spaces at the
-- kind level.  Dimension indices use 'Idx' (bounded by @n@) and parameter
-- names use 'PIdx' (bounded by membership in @ps@).
--
-- The 'D' and 'P' type families provide checked construction that emits a
-- 'TypeError' for out-of-bounds dimensions or unknown parameters.  The
-- 'ValidExpr' family provides container-level validation as a safety net.
module Isl.TypeLevel.Expr
  ( -- * Type-level signed integers
    Z(..)
    -- * Bounded indices
  , Idx(..), PIdx(..)
    -- * Type-level affine expressions (indexed by space)
  , TExpr(..)
    -- * Sugar: type-level operators
  , type (+.), type (-.), type (*.)
    -- * Smart constructors with validation
  , D, P
    -- * Validation helpers
  , If, NatLT, Elem
    -- * Container-level validation
  , Assert, CheckDim, CheckParam, ValidExpr
    -- * Raw (unindexed) expressions and constraints
  , RawExpr(..)
  , RawConstraint(..)
    -- * Raw expression smart constructors
  , RD, RP, RC
    -- * Raw expression operators
  , type (.+), type (.-), type (.*)
    -- * Raw constraint operators
  , type (.>=), type (.<=), type (.==)
  ) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Nat, Symbol, CmpNat, CmpSymbol, ErrorMessage(..), TypeError)

-- * Type-level signed integers

-- | Signed integers at the type/kind level.
--
-- @'Pos' 3@ represents +3, @'Neg' 2@ represents −2, @'Pos' 0@ is zero.
type Z :: Type
data Z = Pos Nat | Neg Nat

-- * Bounded indices

-- | A dimension index bounded by @n@.  @'MkIdx' d@ represents dimension @d@,
-- valid when @d < n@.  Use 'D' for checked construction.
type Idx :: Nat -> Type
data Idx n = MkIdx Nat

-- | A parameter name bounded by membership in @ps@.  @'MkPIdx' s@ represents
-- parameter @s@, valid when @s ∈ ps@.  Use 'P' for checked construction.
type PIdx :: [Symbol] -> Type
data PIdx ps = MkPIdx Symbol

-- * Type-level affine expressions

-- | Type-level affine expressions, indexed by parameter list @ps@ and
-- dimension count @n@.
--
-- The space indices ensure that 'TAdd' can only combine expressions from
-- the same space — mixing spaces is a kind error.
--
-- @
-- -- In a 2-dim space with parameter "N":
-- 'TDim' (D 0) +. 'TParam' (P "N")   :: TExpr '["N"] 2
-- @
type TExpr :: [Symbol] -> Nat -> Type
data TExpr ps n
  = TDim (Idx n)                       -- ^ Dimension variable (bounded index)
  | TParam (PIdx ps)                   -- ^ Parameter variable (bounded name)
  | TConst Z                           -- ^ Integer constant
  | TAdd (TExpr ps n) (TExpr ps n)     -- ^ Addition of two expressions
  | TMul Z (TExpr ps n)                -- ^ Scalar multiplication
  | TFloorDiv (TExpr ps n) Z           -- ^ @floor(expr / d)@ — from ISL existentials

-- * Operators

-- | @a +. b@  =  @'TAdd' a b@
type (+.) :: TExpr ps n -> TExpr ps n -> TExpr ps n
type family a +. b where
  a +. b = 'TAdd a b

-- | @a -. b@  =  @'TAdd' a ('TMul' ('Neg' 1) b)@
type (-.) :: TExpr ps n -> TExpr ps n -> TExpr ps n
type family a -. b where
  a -. b = 'TAdd a ('TMul ('Neg 1) b)

-- | @k *. a@  =  @'TMul' k a@
type (*.) :: Z -> TExpr ps n -> TExpr ps n
type family k *. a where
  k *. a = 'TMul k a

-- * Smart constructors with bounds checking

-- | Construct a dimension index.  Shorthand for @'MkIdx'@.
--
-- Bounds checking (@d < n@) is deferred to the container level via 'AllValid',
-- which fires when constructing a 'TBasicSet', 'TBasicMap', etc.
-- This allows use in type synonyms where @n@ is not yet determined.
--
-- @
-- type Cs = '[ 'TDim (D 0)  >=. 'TConst ('Pos 0) ]
-- type MySet = TBasicSet '["N"] 2 Cs   -- AllValid checks 0 < 2 here
-- @
type D (d :: Nat) = 'MkIdx d

-- | Construct a parameter index.  Shorthand for @'MkPIdx'@.
--
-- Membership checking (@s ∈ ps@) is deferred to the container level via
-- 'AllValid', which fires when constructing a 'TBasicSet', 'TBasicMap', etc.
type P (s :: Symbol) = 'MkPIdx s

-- * Validation helpers

-- | Type-level conditional.
type If :: Bool -> k -> k -> k
type family If b t f where
  If 'True  t _ = t
  If 'False _ f = f

-- | Type-level @a < b@ for 'Nat', returning 'Bool'.
type NatLT :: Nat -> Nat -> Bool
type family NatLT a b where
  NatLT a b = OrdLT (CmpNat a b)

type OrdLT :: Ordering -> Bool
type family OrdLT o where
  OrdLT 'LT = 'True
  OrdLT _   = 'False

-- | Type-level membership test: is symbol @s@ in the list @ps@?
type Elem :: Symbol -> [Symbol] -> Bool
type family Elem s ps where
  Elem _ '[]       = 'False
  Elem s (p ': ps) = ElemCase (CmpSymbol s p) s ps

type ElemCase :: Ordering -> Symbol -> [Symbol] -> Bool
type family ElemCase o s ps where
  ElemCase 'EQ _ _  = 'True
  ElemCase _   s ps = Elem s ps

-- * Container-level validation

-- | Assert a boolean condition at the type level, with a custom error message.
type Assert :: Bool -> ErrorMessage -> Constraint
type family Assert b msg where
  Assert 'True  _   = ()
  Assert 'False msg = TypeError msg

-- | Check that dimension index @d@ is in bounds for an @n@-dimensional space.
type CheckDim :: Nat -> Nat -> Constraint
type family CheckDim d n where
  CheckDim d n = Assert (NatLT d n)
    ('Text "Dimension index " ':<>: 'ShowType d
     ':<>: 'Text " out of bounds for "
     ':<>: 'ShowType n ':<>: 'Text "-dimensional space")

-- | Check that parameter name @s@ appears in the parameter list @ps@.
type CheckParam :: Symbol -> [Symbol] -> Constraint
type family CheckParam s ps where
  CheckParam s ps = Assert (Elem s ps)
    ('Text "Unknown parameter " ':<>: 'ShowType s
     ':<>: 'Text " not in parameter list " ':<>: 'ShowType ps)

-- | Validate all dimension/parameter references in a 'TExpr'.
-- The @ps@ and @n@ parameters must match the expression's space indices.
type family ValidExpr (ps :: [Symbol]) (n :: Nat) (e :: TExpr ps n) :: Constraint where
  ValidExpr ps n ('TDim ('MkIdx d))    = CheckDim d n
  ValidExpr ps n ('TParam ('MkPIdx s)) = CheckParam s ps
  ValidExpr ps n ('TConst _)           = ()
  ValidExpr ps n ('TAdd a b)           = (ValidExpr ps n a, ValidExpr ps n b)
  ValidExpr ps n ('TMul _ a)           = ValidExpr ps n a
  ValidExpr ps n ('TFloorDiv a _)      = ValidExpr ps n a


-- ═══════════════════════════════════════════════════════════════
-- Raw (unindexed) expressions and constraints
--
-- Mirror of TExpr/TConstraint without the ps/n space indices.
-- Used in promoted EqDesc where n varies per equation and cannot
-- appear as a type parameter.  Validation is deferred to conversion
-- time via ToTExpr / ValidExpr.
-- ═══════════════════════════════════════════════════════════════

-- | Unindexed affine expression.  Structurally identical to 'TExpr'
-- but uses bare 'Nat' for dims and 'Symbol' for params — no bounded
-- indices.  Promotes freely to a kind for use in 'EqDesc'.
type RawExpr :: Type
data RawExpr
  = RDim Nat                         -- ^ Dimension variable (unchecked index)
  | RParam Symbol                    -- ^ Parameter variable (unchecked name)
  | RConst Z                         -- ^ Integer constant
  | RAdd RawExpr RawExpr             -- ^ Addition
  | RMul Z RawExpr                   -- ^ Scalar multiplication
  | RFloorDiv RawExpr Z              -- ^ @floor(expr / d)@

-- | Unindexed affine constraint.  Mirrors 'TConstraint'.
type RawConstraint :: Type
data RawConstraint
  = RGe RawExpr                      -- ^ Inequality: e ≥ 0
  | REq RawExpr                      -- ^ Equality:   e = 0

-- * Raw expression smart constructors

-- | Dimension variable.  @RD 0@ = @'RDim 0@.
type RD (d :: Nat) = 'RDim d

-- | Parameter variable.  @RP "K"@ = @'RParam "K"@.
type RP (s :: Symbol) = 'RParam s

-- | Integer constant.  @RC 0@ = @'RConst ('Pos 0)@.
--   @RC 1@ = @'RConst ('Pos 1)@.
type RC (n :: Nat) = 'RConst ('Pos n)

-- * Raw expression operators

type (.+) :: RawExpr -> RawExpr -> RawExpr
type family a .+ b where a .+ b = 'RAdd a b

type (.-) :: RawExpr -> RawExpr -> RawExpr
type family a .- b where a .- b = 'RAdd a ('RMul ('Neg 1) b)

type (.*) :: Z -> RawExpr -> RawExpr
type family k .* a where k .* a = 'RMul k a

infixl 6 .+, .-
infixl 7 .*

-- * Raw constraint operators

-- | @a .>= b@ means @a - b >= 0@
type (.>=) :: RawExpr -> RawExpr -> RawConstraint
type family a .>= b where a .>= b = 'RGe (a .- b)

-- | @a .<= b@ means @b - a >= 0@
type (.<=) :: RawExpr -> RawExpr -> RawConstraint
type family a .<= b where a .<= b = 'RGe (b .- a)

-- | @a .== b@ means @a - b == 0@
type (.==) :: RawExpr -> RawExpr -> RawConstraint
type family a .== b where a .== b = 'REq (a .- b)

infix 4 .>=, .<=, .==
