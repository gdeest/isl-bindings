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
  , ValidConstraint, AllValid, AllValidCSS, AllValidExprs
    -- * Type-level polyhedra
  , TBasicSet(..)
  , TSet(..)
  , TBasicMap(..)
  , TMap(..)
    -- * Type-level affine functions
  , TMultiAff(..)
  , TPwAff(..)
  , TPwMultiAff(..)
    -- * Set proof obligations (solved by isl-plugin)
  , IslSubset(..)
  , IslNonEmpty(..)
  , IslEqual(..)
  , IslDomainOf(..)
    -- * Map proof obligations (solved by isl-plugin)
  , IslMapSubset(..)
  , IslMapEqual(..)
  , IslRangeOf(..)
  , IslImageSubset(..)
    -- * Coverage proof obligations (solved by isl-plugin)
  , IslCovers(..)
  , IslPartitions(..)
    -- * Multi-aff proof obligations (solved by isl-plugin)
  , IslMultiAffEqual(..)
    -- * Parameter precondition marker (consumed by isl-plugin from givens)
  , HasParamCtx(..)
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
    -- * Multi-aff type families (plugin-rewritten)
  , IslMultiAffToMap
  , IslApplyMultiAff
  , IslPreimageMultiAff
  , IslComposeMultiAff
  , IslMultiAffToString
  , IslMultiAffFromString
    -- * PwAff type families (plugin-rewritten)
  , IslSetDimMax
  , IslSetDimMin
    -- * Raw → Typed conversion
  , ToTExpr
  , ToTConstraint
  , ToTConstraints
  , ToTDomain
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

-- Note on the @islProofEv@ methods below
-- ---------------------------------------
-- Each plugin-checked class carries a single nullary method returning @()@.
-- This is NOT used at compile time — the plugin discharges obligations by
-- constructing a dictionary whose method body is @()@ — but it is essential
-- at *runtime* under @-fdefer-type-errors@: an empty class has an empty
-- dictionary, and GHC's simplifier drops empty-dict arguments from proof
-- functions whose bodies don't use them.  When that happens, the deferred
-- 'typeError' binding that @-fdefer-type-errors@ installs for a failing
-- obligation is never demanded, and the runtime trap never fires.  By
-- giving the class a non-trivial method and forcing it at each proof site
-- (@forceObligation \@ps \@n \@cs1 \@cs2@), we ensure the dictionary IS
-- demanded, so the deferred trap fires.  See deviation D15 in
-- @doc/alpha-implementation.md@.

-- | @IslSubset ps n cs1 cs2@ holds iff the polyhedron defined by @cs1@
-- is a subset of (contained in) the polyhedron defined by @cs2@.
--
-- Solved at compile time by the isl-plugin via @isl_set_is_subset@.
class IslSubset (ps :: [Symbol]) (n :: Nat)
                (cs1 :: [TConstraint ps n]) (cs2 :: [TConstraint ps n]) where
  islSubsetEv :: ()

-- | @IslNonEmpty ps n cs@ holds iff the polyhedron defined by @cs@
-- contains at least one integer point (for any valid parameter assignment).
--
-- Solved at compile time by the isl-plugin via @isl_set_is_empty@.
class IslNonEmpty (ps :: [Symbol]) (n :: Nat) (cs :: [TConstraint ps n]) where
  islNonEmptyEv :: ()

-- | @IslEqual ps n cs1 cs2@ holds iff the two polyhedra are equal as sets.
class IslEqual (ps :: [Symbol]) (n :: Nat)
               (cs1 :: [TConstraint ps n]) (cs2 :: [TConstraint ps n]) where
  islEqualEv :: ()

-- | @IslDomainOf ps ni no mapCs domainCs@ holds iff @domainCs@ describes
-- the domain of the map described by @mapCs@.
class IslDomainOf (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                  (mapCs :: [TConstraint ps (ni + no)]) (domainCs :: [TConstraint ps ni]) where
  islDomainOfEv :: ()

-- * Map proof obligations

-- | @IslMapSubset ps ni no cs1 cs2@ holds iff the map defined by @cs1@
-- is a subset of the map defined by @cs2@.
class IslMapSubset (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                   (cs1 :: [TConstraint ps (ni + no)]) (cs2 :: [TConstraint ps (ni + no)]) where
  islMapSubsetEv :: ()

-- | @IslMapEqual ps ni no cs1 cs2@ holds iff the two maps are equal.
class IslMapEqual (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                  (cs1 :: [TConstraint ps (ni + no)]) (cs2 :: [TConstraint ps (ni + no)]) where
  islMapEqualEv :: ()

-- | @IslRangeOf ps ni no mapCs rangeCs@ holds iff @rangeCs@ describes
-- the range of the map described by @mapCs@.
class IslRangeOf (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                 (mapCs :: [TConstraint ps (ni + no)]) (rangeCs :: [TConstraint ps no]) where
  islRangeOfEv :: ()

-- | @IslImageSubset ps ni no mapCs srcCs dstCs@ holds iff the image
-- of @srcCs@ under the map @mapCs@ is a subset of @dstCs@.
class IslImageSubset (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                     (mapCs :: [TConstraint ps (ni + no)])
                     (srcCs :: [TConstraint ps ni]) (dstCs :: [TConstraint ps no]) where
  islImageSubsetEv :: ()


-- * Coverage proof obligations

-- | @IslCovers ps n fullDom branches@ holds iff every integer point in
-- @union(fullDom)@ is contained in @union(branches)@.
--
-- Both arguments are disjunctions (lists of conjunctions).  The check is
-- @isl_set_is_subset(fullDom, union(branches))@.
--
-- This is the pure coverage primitive.  It does NOT check disjointness:
-- the branches may overlap.  For Alpha's branching recurrences use
-- 'IslPartitions' instead — it checks coverage AND pairwise
-- disjointness within the ambient, so each point in the domain is
-- defined by exactly one branch.
class IslCovers (ps :: [Symbol]) (n :: Nat)
                (fullDom :: [[TConstraint ps n]])
                (branches :: [[TConstraint ps n]]) where
  islCoversEv :: ()

-- | @IslPartitions ps n fullDom branches@ holds iff the branch domains
-- (a) cover @fullDom@ and (b) are pairwise disjoint when intersected
-- with @fullDom@.  This is the obligation Alpha's 'Case' emits: every
-- point in the ambient must be defined by exactly one branch.
--
-- The pairwise-within-ambient form matches the "branch guard is
-- implicitly intersected with the ambient" semantic: an overlap of
-- two branches outside @fullDom@ is irrelevant because neither branch
-- fires there.
--
-- Solved at compile time by the isl-plugin: first a subset check for
-- coverage, then pairwise @isl_set_is_empty@ of
-- @bᵢ ∩ bⱼ ∩ fullDom@ for each branch pair.
class IslPartitions (ps :: [Symbol]) (n :: Nat)
                    (fullDom :: [[TConstraint ps n]])
                    (branches :: [[TConstraint ps n]]) where
  islPartitionsEv :: ()


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

-- * Type-level affine functions

-- | Validate all expressions in a list.
type family AllValidExprs (ps :: [Symbol]) (n :: Nat) (es :: [TExpr ps n]) :: Constraint where
  AllValidExprs _  _ '[]       = ()
  AllValidExprs ps n (e ': es) = (ValidExpr ps n e, AllValidExprs ps n es)

-- | A multi-affine function: @ni@ input dims to @no@ output dims.
-- @es@ is a list of output expressions, each a 'TExpr' over the input space.
-- The list length must equal @no@.
data TMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) (es :: [TExpr ps ni]) where
  MkTMultiAff :: AllValidExprs ps ni es => TMultiAff ps ni no es

-- | A piecewise affine expression: list of @(domain constraints, expression)@ pairs.
data TPwAff (ps :: [Symbol]) (n :: Nat) (pieces :: [( [TConstraint ps n], TExpr ps n )]) where
  MkTPwAff :: TPwAff ps n pieces

-- | A piecewise multi-affine function: list of @(domain, output expressions)@ pairs.
data TPwMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
    (pieces :: [( [TConstraint ps ni], [TExpr ps ni] )]) where
  MkTPwMultiAff :: TPwMultiAff ps ni no pieces


-- * Multi-aff proof obligations

-- | @IslMultiAffEqual ps ni no es1 es2@ holds iff the two multi-affs
-- are semantically equal (as verified by ISL).
class IslMultiAffEqual (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
    (es1 :: [TExpr ps ni]) (es2 :: [TExpr ps ni]) where
  islMultiAffEqualEv :: ()


-- * Parameter precondition marker

-- | @HasParamCtx ps cs@ is a marker class that ferries a list of
-- parameter-only constraints @cs@ (over parameter list @ps@, with
-- zero set dimensions) from the user's type signature into the
-- plugin's *given* list.  The plugin's 'solveIsl' scans incoming
-- givens for this class, reifies @cs@, and intersects the
-- corresponding constraints with every ISL set/map built during
-- wanted discharge — giving the solver access to assumptions like
-- @N ≥ 1@ when verifying subset / image / coverage obligations.
--
-- There is no instance for this class.  Users must place
-- @HasParamCtx ps cs@ in the type signature of any binding that
-- needs the precondition; GHC then hands the constraint to the
-- plugin as a given when elaborating the body.  See deviation D19
-- in @doc/alpha-implementation.md@ (v3 plan) for the full story.
class HasParamCtx (ps :: [Symbol]) (cs :: [TConstraint ps 0]) where
  hasParamCtxEv :: ()


-- * Multi-aff type families (plugin-rewritten)

-- | Convert a multi-aff to map constraints.
-- For each output dim @j@, emits @dim(ni+j) = expr_j(inputs)@.
type family IslMultiAffToMap (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
    (es :: [TExpr ps ni]) :: [TConstraint ps (ni + no)]

-- | Apply a multi-aff to a set (functional image). Returns disjunctive result.
type family IslApplyMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
    (es :: [TExpr ps ni]) (setCs :: [TConstraint ps ni]) :: [[TConstraint ps no]]

-- | Preimage of a set under a multi-aff (single conjunction).
-- Given @f : Z^ni -> Z^no@ (expressed as @no@ output expressions over @ni@ dims)
-- and @S ⊆ Z^no@, computes @{x ∈ Z^ni : f(x) ∈ S}@.
-- Returns a single conjunction.  For affine maps (the common case in
-- polyhedral transforms), the preimage of a basic set is always a basic set.
-- Wraps @isl_set_preimage_multi_aff@ + coalesce.
type family IslPreimageMultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
    (es :: [TExpr ps ni]) (setCs :: [TConstraint ps no]) :: [TConstraint ps ni]

-- | Compose two multi-affs: @(nk -> no) after (ni -> nk) = (ni -> no)@.
-- Returns the output expression list for the composed function.
type family IslComposeMultiAff (ps :: [Symbol]) (ni :: Nat) (nk :: Nat) (no :: Nat)
    (es1 :: [TExpr ps nk]) (es2 :: [TExpr ps ni]) :: [TExpr ps ni]

-- | Render a multi-aff to its ISL string representation.
type family IslMultiAffToString (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
    (es :: [TExpr ps ni]) :: Symbol

-- | Parse a multi-aff from its ISL string representation.
type family IslMultiAffFromString (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
    (s :: Symbol) :: [TExpr ps ni]


-- * PwAff type families (plugin-rewritten)

-- | Maximum value of dimension @d@ in a set, as piecewise affine pieces.
type family IslSetDimMax (ps :: [Symbol]) (n :: Nat) (d :: Nat)
    (cs :: [[TConstraint ps n]]) :: [( [TConstraint ps n], TExpr ps n )]

-- | Minimum value of dimension @d@ in a set, as piecewise affine pieces.
type family IslSetDimMin (ps :: [Symbol]) (n :: Nat) (d :: Nat)
    (cs :: [[TConstraint ps n]]) :: [( [TConstraint ps n], TExpr ps n )]


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


-- ═══════════════════════════════════════════════════════════════
-- Raw → Typed conversion
--
-- Convert unindexed RawExpr/RawConstraint to space-indexed
-- TExpr/TConstraint.  Validation (dim bounds, param membership)
-- is NOT performed here — use ValidExpr on the result.
-- ═══════════════════════════════════════════════════════════════

-- | Convert a 'RawExpr' to a 'TExpr' by wrapping dim/param references
-- in 'MkIdx'/'MkPIdx'.
type family ToTExpr (ps :: [Symbol]) (n :: Nat) (r :: RawExpr) :: TExpr ps n where
  ToTExpr ps n ('RDim d)        = 'TDim ('MkIdx d)
  ToTExpr ps n ('RParam s)      = 'TParam ('MkPIdx s)
  ToTExpr ps n ('RConst z)      = 'TConst z
  ToTExpr ps n ('RAdd a b)      = 'TAdd (ToTExpr ps n a) (ToTExpr ps n b)
  ToTExpr ps n ('RMul k a)      = 'TMul k (ToTExpr ps n a)
  ToTExpr ps n ('RFloorDiv a d) = 'TFloorDiv (ToTExpr ps n a) d

-- | Convert a single 'RawConstraint' to 'TConstraint'.
type family ToTConstraint (ps :: [Symbol]) (n :: Nat) (rc :: RawConstraint) :: TConstraint ps n where
  ToTConstraint ps n ('RGe e) = 'TGe (ToTExpr ps n e)
  ToTConstraint ps n ('REq e) = 'TEq (ToTExpr ps n e)

-- | Convert a conjunction (list) of raw constraints.
type family ToTConstraints (ps :: [Symbol]) (n :: Nat) (rcs :: [RawConstraint]) :: [TConstraint ps n] where
  ToTConstraints _  _ '[]        = '[]
  ToTConstraints ps n (rc ': cs) = ToTConstraint ps n rc ': ToTConstraints ps n cs

-- | Convert a disjunction of conjunctions (full domain).
type family ToTDomain (ps :: [Symbol]) (n :: Nat) (dom :: [[RawConstraint]]) :: [[TConstraint ps n]] where
  ToTDomain _  _ '[]           = '[]
  ToTDomain ps n (conj ': css) = ToTConstraints ps n conj ': ToTDomain ps n css
