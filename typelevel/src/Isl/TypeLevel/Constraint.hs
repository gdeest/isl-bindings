{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

-- | Type-level polyhedral constraints and the proof-obligation classes
-- that the GHC plugin knows how to solve.
--
-- A type-level basic set is a list of 'TConstraint's (a conjunction).
-- A type-level set is a list of conjunctions (a disjunction).
--
-- All dimension indices and parameter references are validated at compile
-- time via 'AllValid' — using @'TDim' 5@ in a 2-dimensional space or
-- @'TParam' "Q"@ when @ps = \'["N"]@ produces a clear 'TypeError'.
--
-- The classes 'IslSubset', 'IslNonEmpty', etc. express properties that
-- the isl-plugin typechecker plugin can verify at compile time by calling
-- ISL under the hood.
module Isl.TypeLevel.Constraint
  ( -- * Type-level constraints
    TConstraint(..)
    -- * Sugar: type-level comparison operators
  , type (>=.), type (<=.), type (==.)
    -- * Validation
  , ValidConstraint, AllValid, AllValidCSS
    -- * Type-level polyhedra (validated)
  , TBasicSet(..)
  , TSet(..)
  , TBasicMap(..)
    -- * Proof obligations (solved by isl-plugin)
  , IslSubset
  , IslNonEmpty
  , IslEqual
  , IslDomainOf
  ) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Nat, Symbol, type (+))

import Isl.TypeLevel.Expr

-- | A single affine constraint (equality or inequality).
--
-- Follows ISL convention:
--
--   * @'TEq' e@ means @e = 0@
--   * @'TGe' e@ means @e ≥ 0@
data TConstraint
  = TEq TExpr   -- ^ Equality:   e = 0
  | TGe TExpr   -- ^ Inequality: e ≥ 0

-- | @a >=. b@  ⟹  @'TGe' (a -. b)@   i.e. a − b ≥ 0
type family (a :: TExpr) >=. (b :: TExpr) :: TConstraint where
  a >=. b = 'TGe (a -. b)

-- | @a <=. b@  ⟹  @'TGe' (b -. a)@
type family (a :: TExpr) <=. (b :: TExpr) :: TConstraint where
  a <=. b = 'TGe (b -. a)

-- | @a ==. b@  ⟹  @'TEq' (a -. b)@
type family (a :: TExpr) ==. (b :: TExpr) :: TConstraint where
  a ==. b = 'TEq (a -. b)

-- * Validation

-- | Validate a single 'TConstraint' against parameter list @ps@ and
-- dimension count @n@.
type family ValidConstraint (ps :: [Symbol]) (n :: Nat) (c :: TConstraint) :: Constraint where
  ValidConstraint ps n ('TEq e) = ValidExpr ps n e
  ValidConstraint ps n ('TGe e) = ValidExpr ps n e

-- | Validate all constraints in a conjunction (list).
-- Every dimension index must be @< n@ and every parameter name must be in @ps@.
type family AllValid (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint]) :: Constraint where
  AllValid _  _ '[]       = ()
  AllValid ps n (c ': cs) = (ValidConstraint ps n c, AllValid ps n cs)

-- | Validate all constraints in a disjunction (list of conjunctions).
type family AllValidCSS (ps :: [Symbol]) (n :: Nat) (css :: [[TConstraint]]) :: Constraint where
  AllValidCSS _  _ '[]         = ()
  AllValidCSS ps n (cs ': css) = (AllValid ps n cs, AllValidCSS ps n css)

-- * Type-level polyhedra

-- | A basic set: parameter names, dimension count, and a validated conjunction
-- of constraints defining a single convex polyhedron.
--
-- The 'MkTBasicSet' constructor requires 'AllValid' — you cannot construct
-- a 'TBasicSet' with out-of-bounds dimension indices or unknown parameters.
--
-- @
-- type Triangle = 'TBasicSet' '["N"] 2
--   '[ 'TDim 0  >=. 'TConst ('Pos 0)
--    , 'TDim 0  <=. 'TParam "N"
--    , 'TDim 1  >=. 'TConst ('Pos 0)
--    , 'TDim 1  <=. 'TDim 0
--    ]
-- @
type TBasicSet :: [Symbol] -> Nat -> [TConstraint] -> Type
data TBasicSet ps n cs where
  MkTBasicSet :: AllValid ps n cs => TBasicSet ps n cs

-- | A set: a disjunction of conjunctions (union of convex polyhedra).
type TSet :: [Symbol] -> Nat -> [[TConstraint]] -> Type
data TSet ps n css where
  MkTSet :: AllValidCSS ps n css => TSet ps n css

-- | A basic map: input dims, output dims, constraints over both.
-- Dimensions 0..ni−1 are inputs, ni..ni+no−1 are outputs in the constraint
-- indexing (matching ISL's convention for map spaces).
type TBasicMap :: [Symbol] -> Nat -> Nat -> [TConstraint] -> Type
data TBasicMap ps ni no cs where
  MkTBasicMap :: AllValid ps (ni + no) cs => TBasicMap ps ni no cs


-- * Proof obligations — the plugin solves these

-- | @IslSubset ps n cs1 cs2@ holds iff the polyhedron defined by @cs1@
-- is a subset of (contained in) the polyhedron defined by @cs2@.
--
-- Solved at compile time by the isl-plugin via @isl_set_is_subset@.
type IslSubset :: [Symbol] -> Nat -> [TConstraint] -> [TConstraint] -> Constraint
class IslSubset ps n cs1 cs2

-- | @IslNonEmpty ps n cs@ holds iff the polyhedron defined by @cs@
-- contains at least one integer point (for any valid parameter assignment).
--
-- Solved at compile time by the isl-plugin via @isl_set_is_empty@.
type IslNonEmpty :: [Symbol] -> Nat -> [TConstraint] -> Constraint
class IslNonEmpty ps n cs

-- | @IslEqual ps n cs1 cs2@ holds iff the two polyhedra are equal as sets.
type IslEqual :: [Symbol] -> Nat -> [TConstraint] -> [TConstraint] -> Constraint
class IslEqual ps n cs1 cs2

-- | @IslDomainOf ps ni no mapCs domainCs@ holds iff @domainCs@ describes
-- the domain of the map described by @mapCs@.
type IslDomainOf :: [Symbol] -> Nat -> Nat -> [TConstraint] -> [TConstraint] -> Constraint
class IslDomainOf ps ni no mapCs domainCs
