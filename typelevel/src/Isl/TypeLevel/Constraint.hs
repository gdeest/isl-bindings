{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

-- | Type-level polyhedral constraints and proof-obligation classes.
--
-- Constraints are indexed by their parameter list @ps@ and dimension count @n@,
-- ensuring that all expressions within a constraint live in the same space.
--
-- The 'TBasicSet', 'TSet', 'TBasicMap', 'TMap' GADTs require 'AllValid',
-- acting as a safety net that catches any direct use of @'MkIdx'@ / @'MkPIdx'@
-- that bypasses the checked 'D' / 'P' constructors.
module Isl.TypeLevel.Constraint
  ( -- * Type-level constraints (indexed by space)
    TConstraint(..)
    -- * Sugar: type-level comparison operators
  , type (>=.), type (<=.), type (==.)
    -- * Validation
  , ValidConstraint, AllValid, AllValidCSS
    -- * Type-level polyhedra
  , TBasicSet(..)
  , TSet(..)
  , TBasicMap(..)
  , TMap(..)
    -- * Set proof obligations (solved by isl-plugin)
  , IslSubset
  , IslNonEmpty
  , IslEqual
  , IslDomainOf
    -- * Map proof obligations (solved by isl-plugin)
  , IslMapSubset
  , IslMapEqual
  , IslRangeOf
  , IslImageSubset
    -- * Type-level ISL computations (plugin-rewritten type families)
  , IslIntersectSet
  , IslComplementSet
  , IslDifferenceSet
  , IslApply
  , IslDomainTF
  , IslRangeTF
  , IslCompose
  , IslReverseMap
  , IslProjectOut
  , IslFromString
  , IslToString
  , IslMapToString
  ) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Nat, Symbol, type (+))

import Isl.TypeLevel.Expr

-- | A single affine constraint (equality or inequality), indexed by
-- parameter list @ps@ and dimension count @n@.
--
-- Follows ISL convention:
--
--   * @'TEq' e@ means @e = 0@
--   * @'TGe' e@ means @e ≥ 0@
type TConstraint :: [Symbol] -> Nat -> Type
data TConstraint ps n
  = TEq (TExpr ps n)   -- ^ Equality:   e = 0
  | TGe (TExpr ps n)   -- ^ Inequality: e ≥ 0

-- | @a >=. b@  ⟹  @'TGe' (a -. b)@   i.e. a − b ≥ 0
type (>=.) :: TExpr ps n -> TExpr ps n -> TConstraint ps n
type family a >=. b where
  a >=. b = 'TGe (a -. b)

-- | @a <=. b@  ⟹  @'TGe' (b -. a)@
type (<=.) :: TExpr ps n -> TExpr ps n -> TConstraint ps n
type family a <=. b where
  a <=. b = 'TGe (b -. a)

-- | @a ==. b@  ⟹  @'TEq' (a -. b)@
type (==.) :: TExpr ps n -> TExpr ps n -> TConstraint ps n
type family a ==. b where
  a ==. b = 'TEq (a -. b)

-- * Validation

-- | Validate a single 'TConstraint' against parameter list @ps@ and
-- dimension count @n@.
type family ValidConstraint (ps :: [Symbol]) (n :: Nat) (c :: TConstraint ps n) :: Constraint where
  ValidConstraint ps n ('TEq e) = ValidExpr ps n e
  ValidConstraint ps n ('TGe e) = ValidExpr ps n e

-- | Validate all constraints in a conjunction (list).
-- Every dimension index must be @< n@ and every parameter name must be in @ps@.
type family AllValid (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint ps n]) :: Constraint where
  AllValid _  _ '[]       = ()
  AllValid ps n (c ': cs) = (ValidConstraint ps n c, AllValid ps n cs)

-- | Validate all constraints in a disjunction (list of conjunctions).
type family AllValidCSS (ps :: [Symbol]) (n :: Nat) (css :: [[TConstraint ps n]]) :: Constraint where
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
-- type Triangle = TBasicSet '["N"] 2
--   '[ 'TDim (D 0)  >=. 'TConst ('Pos 0)
--    , 'TDim (D 0)  <=. 'TParam (P "N")
--    , 'TDim (D 1)  >=. 'TConst ('Pos 0)
--    , 'TDim (D 1)  <=. 'TDim (D 0)
--    ]
-- @
data TBasicSet (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint ps n]) where
  MkTBasicSet :: AllValid ps n cs => TBasicSet ps n cs

-- | A set: a disjunction of conjunctions (union of convex polyhedra).
data TSet (ps :: [Symbol]) (n :: Nat) (css :: [[TConstraint ps n]]) where
  MkTSet :: AllValidCSS ps n css => TSet ps n css

-- | A basic map: input dims, output dims, constraints over both.
-- Dimensions 0..ni−1 are inputs, ni..ni+no−1 are outputs in the constraint
-- indexing (matching ISL's convention for map spaces).
type TBasicMap :: forall (ps :: [Symbol]) -> forall (ni :: Nat) -> forall (no :: Nat) -> [TConstraint ps (ni + no)] -> Type
data TBasicMap ps ni no cs where
  MkTBasicMap :: AllValid ps (ni + no) cs => TBasicMap ps ni no cs

-- | A map: a disjunction of conjunctions over the combined input/output space.
type TMap :: forall (ps :: [Symbol]) -> forall (ni :: Nat) -> forall (no :: Nat) -> [[TConstraint ps (ni + no)]] -> Type
data TMap ps ni no css where
  MkTMap :: AllValidCSS ps (ni + no) css => TMap ps ni no css


-- * Set proof obligations — the plugin solves these

-- | @IslSubset ps n cs1 cs2@ holds iff the polyhedron defined by @cs1@
-- is a subset of (contained in) the polyhedron defined by @cs2@.
--
-- Solved at compile time by the isl-plugin via @isl_set_is_subset@.
class IslSubset (ps :: [Symbol]) (n :: Nat)
                (cs1 :: [TConstraint ps n]) (cs2 :: [TConstraint ps n])

-- | @IslNonEmpty ps n cs@ holds iff the polyhedron defined by @cs@
-- contains at least one integer point (for any valid parameter assignment).
--
-- Solved at compile time by the isl-plugin via @isl_set_is_empty@.
class IslNonEmpty (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint ps n])

-- | @IslEqual ps n cs1 cs2@ holds iff the two polyhedra are equal as sets.
class IslEqual (ps :: [Symbol]) (n :: Nat)
               (cs1 :: [TConstraint ps n]) (cs2 :: [TConstraint ps n])

-- | @IslDomainOf ps ni no mapCs domainCs@ holds iff @domainCs@ describes
-- the domain of the map described by @mapCs@.
class IslDomainOf (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                  (mapCs :: [TConstraint ps (ni + no)]) (domainCs :: [TConstraint ps ni])

-- * Map proof obligations

-- | @IslMapSubset ps ni no cs1 cs2@ holds iff the map defined by @cs1@
-- is a subset of the map defined by @cs2@.
class IslMapSubset (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                   (cs1 :: [TConstraint ps (ni + no)]) (cs2 :: [TConstraint ps (ni + no)])

-- | @IslMapEqual ps ni no cs1 cs2@ holds iff the two maps are equal.
class IslMapEqual (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                  (cs1 :: [TConstraint ps (ni + no)]) (cs2 :: [TConstraint ps (ni + no)])

-- | @IslRangeOf ps ni no mapCs rangeCs@ holds iff @rangeCs@ describes
-- the range of the map described by @mapCs@.
class IslRangeOf (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                 (mapCs :: [TConstraint ps (ni + no)]) (rangeCs :: [TConstraint ps no])

-- | @IslImageSubset ps ni no mapCs srcCs dstCs@ holds iff the image
-- of @srcCs@ under the map @mapCs@ is a subset of @dstCs@.
class IslImageSubset (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                     (mapCs :: [TConstraint ps (ni + no)])
                     (srcCs :: [TConstraint ps ni]) (dstCs :: [TConstraint ps no])


-- * Type-level ISL computations
-- These are "stuck" type families with no equations.
-- The isl-plugin rewrites them at compile time by calling ISL.

-- | Intersection of two basic sets. Result is a disjunction (ISL may simplify).
type family IslIntersectSet (ps :: [Symbol]) (n :: Nat)
  (cs1 :: [TConstraint ps n]) (cs2 :: [TConstraint ps n]) :: [[TConstraint ps n]]

-- | Complement of a basic set. Result is a disjunction.
type family IslComplementSet (ps :: [Symbol]) (n :: Nat)
  (cs :: [TConstraint ps n]) :: [[TConstraint ps n]]

-- | Difference of two basic sets. Result is a disjunction.
type family IslDifferenceSet (ps :: [Symbol]) (n :: Nat)
  (cs1 :: [TConstraint ps n]) (cs2 :: [TConstraint ps n]) :: [[TConstraint ps n]]

-- | Image of a set under a map: apply map to set.
type family IslApply (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
  (mapCs :: [TConstraint ps (ni + no)]) (setCs :: [TConstraint ps ni]) :: [[TConstraint ps no]]

-- | Domain of a map.
type family IslDomainTF (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
  (mapCs :: [TConstraint ps (ni + no)]) :: [[TConstraint ps ni]]

-- | Range of a map.
type family IslRangeTF (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
  (mapCs :: [TConstraint ps (ni + no)]) :: [[TConstraint ps no]]

-- | Composition of two maps: m1 after m2.
type family IslCompose (ps :: [Symbol]) (ni :: Nat) (nk :: Nat) (no :: Nat)
  (m1Cs :: [TConstraint ps (nk + no)]) (m2Cs :: [TConstraint ps (ni + nk)])
  :: [[TConstraint ps (ni + no)]]

-- | Reverse a map (swap domain and range).
type family IslReverseMap (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
  (mapCs :: [TConstraint ps (ni + no)]) :: [[TConstraint ps (no + ni)]]

-- | Project out dimensions via Fourier-Motzkin elimination.
-- @IslProjectOut ps n nResult first count cs@ projects out @count@ set
-- dimensions starting at position @first@, yielding a set in @nResult@ dims
-- (where @nResult = n - count@).
--
-- This is existential quantification: if @S ⊆ Z^n@, then
-- @project_out(S, first, count)@ is @{ y | ∃x. (y_0..y_{first-1}, x, y_{first}..y_{nResult-1}) ∈ S }@
type family IslProjectOut (ps :: [Symbol]) (n :: Nat) (nResult :: Nat)
  (first :: Nat) (count :: Nat)
  (cs :: [TConstraint ps n]) :: [[TConstraint ps nResult]]

-- | Parse an ISL set from its string representation.
-- This is the universal escape hatch: any ISL set syntax is supported,
-- including existential quantification, modular constraints, floor/ceiling, etc.
--
-- @
-- type DivBy3 = IslFromString '["N"] 1 "[N] -> { [i] : exists k: i = 3k and 0 <= i <= N }"
-- @
type family IslFromString (ps :: [Symbol]) (n :: Nat) (s :: Symbol)
  :: [[TConstraint ps n]]


-- | Render a set (given as type-level constraints) to its ISL string
-- representation, returned as a type-level 'Symbol'.
--
-- Use in GHCi:
--
-- @
-- >>> :kind! IslToString '["N"] 2 Triangle
-- IslToString '["N"] 2 Triangle :: Symbol
-- = "[N] -> { [i0, i1] : 0 <= i0 <= N and 0 <= i1 <= i0 }"
-- @
type family IslToString (ps :: [Symbol]) (n :: Nat)
  (cs :: [TConstraint ps n]) :: Symbol

-- | Render a map to its ISL string representation.
type family IslMapToString (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
  (cs :: [TConstraint ps (ni + no)]) :: Symbol

-- * Modular constraint sugar
--
-- These produce ISL string representations for common patterns involving
-- modular arithmetic / divisibility.  Use with 'IslFromString' or
-- directly as set definitions.

-- | @Divisible d m@ represents @∃k. dim_d = k * m@, i.e., dimension @d@
-- is divisible by @m@.  Intended for use with 'IslIntersectSet':
--
-- @
-- type EvenRows = IslFromString '["N"] 2
--   "[N] -> { [i, j] : exists k: i = 2k }"
-- @
--
-- As a standalone type synonym (for documentation):
type Divisible (d :: Nat) (m :: Nat) = '()  -- placeholder; use IslFromString

-- | @ModEq d m r@ represents @dim_d mod m = r@.
-- As a standalone type synonym (for documentation):
type ModEq (d :: Nat) (m :: Nat) (r :: Nat) = '()  -- placeholder; use IslFromString
