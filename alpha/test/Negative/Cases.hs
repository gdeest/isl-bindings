{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
-- -Wno-deferred-type-errors deliberately *not* set: deferred warnings
-- must appear in the build log so we can audit which obligations are
-- being deferred.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Negative type-safety tests.
--
-- Each top-level binding in this module is a deliberately *incorrect*
-- Alpha program.  The test harness in @Main.hs@ forces each binding
-- via 'Control.Exception.evaluate' and asserts that a 'TypeError'
-- exception is raised.  If the binding compiles to a value (rather
-- than to a deferred type error), the negative test fails — the type
-- system did not catch the bug.
module Negative.Cases
  ( forceBadOutOfBoundsK
  , forceBadRankMismatch
  , forceBadUndeclaredVar
  , forceBadMissingDef
  , forceBadDoubleDef
  , forceBadBrokenCausalInCases
  , forceBadCholeskyCoverageGap
  , forceBadCholeskyBranchOutOfBounds
  , forceBadContradictoryPctx
  , forceBadLUDiagonalRead
  , forceBadHeat3DOutOfBoundsNeighbor
  , forceBadCasePartitionsCoverageGap
  , forceBadCasePartitionsNonDisjoint
  , forceBadReducePartial
  ) where

import Control.Exception (evaluate)
import Data.Proxy (Proxy(..))

import Alpha.Surface.Core
  ( Decl(..)
  , DeclList(..)
  , Decls(..)
  , DefinesAllExactlyOnce(..)
  , Equation(..)
  , EqList(..)
  , Expr(..)
  , System
  , pattern System
  , VarDecl(..)
  , type (++)
  )
import Examples.Matmul
  ( CubeN
  , MatmulDecls
  , MatmulInputs
  , MatmulOutputs
  , MatmulLocals
  , SquareN
  )
import Examples.Cholesky
  ( DiagN
  , LowerTri
  , StrictLowerN
  )
import Examples.LU
  ( LStrict
  , UBody3D
  )
import Examples.Heat3D
  ( Interior
  , TimeBox
  )
import Isl.TypeLevel.Constraint
  ( IslCovers(..)
  , IslImageEqual(..)
  , IslImageSubset(..)
  , IslMultiAffToMap
  , IslNonEmpty(..)
  , IslPartitions(..)
  , IslSubset(..)
  , TConstraint
  , type (>=.)
  , type (<=.)
  , type (==.)
  )
import Isl.TypeLevel.Expr
  ( D
  , P
  , TExpr(..)
  , Z(..)
  , type (-.)
  , type (+.)
  )
import Isl.TypeLevel.Reflection (DomTag(..))


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #1: out-of-bounds access (k+1 access from a body where k=N-1)
-- ═══════════════════════════════════════════════════════════════════════
--
-- The body iterates over the 3D cube CubeN { 0 ≤ i,j,k ≤ N-1 }, and the
-- access pattern reads A[i, k+1].  At k = N-1, the access lands at
-- index k+1 = N, which is outside A's declared domain { 0 ≤ i,k ≤ N-1 }.
-- The plugin's IslImageSubset obligation should reject this at type
-- checking time.

-- The "out-of-bounds" test substitutes a 1D-vs-2D rank mismatch which
-- fires as a structural kind error.  GHC's cascading-error recovery
-- can suppress plugin-discharged obligations when other type errors
-- are present in the same module, so the real IslSubset-based
-- out-of-bounds test lives in @Negative/Minimal.hs@ in its own module.
type Bad1DAccess =
  '[ 'TDim (D 0)
   ] :: [TExpr '["N"] 3]

forceBadOutOfBoundsK :: () -> IO ()
forceBadOutOfBoundsK () =
  let !badExpr = Dep
                   (Proxy :: Proxy (IslMultiAffToMap '["N"] 3 1 Bad1DAccess))
                   (Var (Proxy @"A"))
                :: Expr '["N"] MatmulDecls 3 ('Literal CubeN) Double
   in badExpr `seq` return ()

-- In-place IslSubset proof: when this binding sits alongside
-- type-family TypeError bindings in the same module, GHC suppresses
-- the compile-time warning.  The deferred typeError trap is still
-- inserted so the runtime evaluation below still fires.

type CausalMaskInv =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 1)
   , 'TDim (D 0)     >=. 'TDim (D 1)
   ] :: [TConstraint '["S","W"] 2]

type BrokenCausalMaskInv =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "S") >=. 'TDim (D 1)
   , 'TDim (D 1)     >=. 'TDim (D 0)
   ] :: [TConstraint '["S","W"] 2]


proofBrokenInCausalInv
  :: IslSubset '["S","W"] 2 BrokenCausalMaskInv CausalMaskInv => ()
proofBrokenInCausalInv =
  -- Force the class dictionary by calling one of its methods so the
  -- deferred typeError binding GHC armed for this obligation is
  -- actually demanded at runtime.  Without this, the optimizer drops
  -- the dict argument and the runtime trap never fires.
  islSubsetEv @'["S","W"] @2 @BrokenCausalMaskInv @CausalMaskInv

_useBrokenInCausalInv :: ()
_useBrokenInCausalInv = proofBrokenInCausalInv

forceBadBrokenCausalInCases :: () -> IO ()
forceBadBrokenCausalInCases () = do
  _ <- evaluate _useBrokenInCausalInv
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #2: rank mismatch (1D access of a 2D variable)
-- ═══════════════════════════════════════════════════════════════════════

forceBadRankMismatch :: () -> IO ()
forceBadRankMismatch () =
  -- Same construction as #1 above; both produce a kind/dim mismatch
  -- error.
  let !badExpr = Dep
                   (Proxy :: Proxy (IslMultiAffToMap '["N"] 3 1 Bad1DAccess))
                   (Var (Proxy @"A"))
                :: Expr '["N"] MatmulDecls 3 ('Literal CubeN) Double
   in badExpr `seq` return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #3: undeclared variable
-- ═══════════════════════════════════════════════════════════════════════

forceBadUndeclaredVar :: () -> IO ()
forceBadUndeclaredVar () =
  let !badExpr = Var (Proxy @"Q")
                :: Expr '["N"] MatmulDecls 2 ('Literal SquareN) Double
   in badExpr `seq` return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #4: missing equation definition
-- ═══════════════════════════════════════════════════════════════════════

forceBadMissingDef :: () -> IO ()
forceBadMissingDef () =
  let !_ = definesAllExactlyOnceEv
             @'["N"] @(MatmulOutputs ++ MatmulLocals) @'[]
   in return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #5: double equation definition
-- ═══════════════════════════════════════════════════════════════════════

forceBadDoubleDef :: () -> IO ()
forceBadDoubleDef () =
  let !_ = definesAllExactlyOnceEv
             @'["N"] @(MatmulOutputs ++ MatmulLocals) @'["C", "C"]
   in return ()

trivialBody :: Expr '["N"] MatmulDecls 2 ('Literal SquareN) Double
trivialBody = Const 0


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #6: cholesky coverage gap
-- ═══════════════════════════════════════════════════════════════════════
--
-- The cholesky body's 'Case' must have branches whose union covers the
-- full 'LowerTri' domain.  Removing the diagonal branch leaves a gap
-- (the line @i == j@) that 'StrictLowerN' alone does not reach.  The
-- plugin's 'IslCovers' handler should reject this.
--
-- Encoded as a direct 'IslCovers' proof rather than building a full
-- malformed 'System' value, since that's the smallest surface that
-- exercises the obligation.  Uses the force-method pattern so the
-- deferred runtime trap fires under '-fdefer-type-errors'.

proofBadCholeskyCoverage
  :: IslCovers '["N"] 2 '[LowerTri] '[StrictLowerN] => ()
proofBadCholeskyCoverage =
  islCoversEv @'["N"] @2 @'[LowerTri] @'[StrictLowerN]

_useBadCholeskyCoverage :: ()
_useBadCholeskyCoverage = proofBadCholeskyCoverage

forceBadCholeskyCoverageGap :: () -> IO ()
forceBadCholeskyCoverageGap () = do
  _ <- evaluate _useBadCholeskyCoverage
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #7: cholesky branch out of bounds
-- ═══════════════════════════════════════════════════════════════════════
--
-- The 'BCons' constructor requires @IslSubsetD ps n d amb@ — each branch
-- domain must be a subset of the ambient domain ('LowerTri' in cholesky).
-- Using 'SquareN' (the full N-by-N square) as a branch domain violates
-- this: the upper triangle of 'SquareN' is not contained in 'LowerTri'.
--
-- Encoded as a direct 'IslSubset' proof between the underlying constraint
-- lists.  Uses the force-method pattern.

proofBadCholeskyBranchBound
  :: IslSubset '["N"] 2 SquareN LowerTri => ()
proofBadCholeskyBranchBound =
  islSubsetEv @'["N"] @2 @SquareN @LowerTri

_useBadCholeskyBranchBound :: ()
_useBadCholeskyBranchBound = proofBadCholeskyBranchBound

forceBadCholeskyBranchOutOfBounds :: () -> IO ()
forceBadCholeskyBranchOutOfBounds () = do
  _ <- evaluate _useBadCholeskyBranchBound
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #8b: contradictory pctx — IslNonEmpty fails on System
-- construction
-- ═══════════════════════════════════════════════════════════════════════
--
-- With strict pctx semantics, the System constructor's superclass
-- @IslNonEmpty ps 0 pctx@ forces the plugin to prove that some
-- parameter assignment satisfies pctx.  Declaring
-- @'[ N >= 1, N <= 0 ]@ is a contradiction; the plugin rejects
-- the obligation, and the deferred trap fires when we force the proof.

type BadPctx =
  '[ 'TParam (P "N") >=. 'TConst ('Pos 1)
   , 'TParam (P "N") <=. 'TConst ('Pos 0)
   ] :: [TConstraint '["N"] 0]

proofBadContradictoryPctx
  :: IslNonEmpty '["N"] 0 BadPctx => ()
proofBadContradictoryPctx =
  islNonEmptyEv @'["N"] @0 @BadPctx

_useBadContradictoryPctx :: ()
_useBadContradictoryPctx = proofBadContradictoryPctx

forceBadContradictoryPctx :: () -> IO ()
forceBadContradictoryPctx () = do
  _ <- evaluate _useBadContradictoryPctx
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #9: LU reads L on its own diagonal
-- ═══════════════════════════════════════════════════════════════════════
--
-- LU's @L@ is *strict* lower triangular in the Doolittle convention:
-- its declared domain is @LStrict = {(i, j) : 0 ≤ j ≤ i - 1}@, so
-- the diagonal @L[i, i]@ is NOT stored (it's implicitly 1).  A
-- variant of the U equation's 3D reduction body that tries to read
-- @L[i, i]@ instead of @L[i, k]@ emits an 'IslImageSubsetD'
-- obligation with the map @(i, j, k) → (i, i)@.  The image from
-- @UBody3D = {0 ≤ i ≤ j ≤ N-1, 0 ≤ k ≤ i-1}@ is
-- @{(i, i) : 1 ≤ i ≤ N-1}@ (non-empty because @UBody3D@ is
-- non-empty for @N ≥ 2@, and at those parameter values
-- @k@-existence forces @i ≥ 1@).  That set is not a subset of
-- @LStrict@ because @(i, i)@ has @b = i@ and @a = i@, and
-- @b ≤ a - 1@ requires @i ≤ i - 1@, which is false.  The plugin
-- rejects the obligation.
--
-- Uses the force-method pattern: a proof binding whose only job is
-- to invoke 'islImageSubsetEv' at the failing instantiation, and a
-- top-level '_useBadLUDiagonalRead' that forces the proof to demand
-- the class dictionary, firing the deferred typeError trap the
-- plugin armed.

-- Access map that reads @L@ at the diagonal from a 3D body:
-- @(i, j, k) → (i, i)@.  Same shape as 'Examples.LU.AccessLIK'
-- but with @D 0@ in the second slot instead of @D 2@.
type AccessLDiag =
  '[ 'TDim (D 0)
   , 'TDim (D 0)
   ] :: [TExpr '["N"] 3]

proofBadLUDiagonalRead
  :: IslImageSubset '["N"] 3 2
       (IslMultiAffToMap '["N"] 3 2 AccessLDiag)
       UBody3D
       LStrict
  => ()
proofBadLUDiagonalRead =
  islImageSubsetEv
    @'["N"] @3 @2
    @(IslMultiAffToMap '["N"] 3 2 AccessLDiag)
    @UBody3D
    @LStrict

_useBadLUDiagonalRead :: ()
_useBadLUDiagonalRead = proofBadLUDiagonalRead

forceBadLUDiagonalRead :: () -> IO ()
forceBadLUDiagonalRead () = do
  _ <- evaluate _useBadLUDiagonalRead
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #10: Heat3D out-of-bounds neighbour read
-- ═══════════════════════════════════════════════════════════════════════
--
-- Heat3D's interior body reads @u[t-1, i-1, j, k]@ (and five other
-- neighbours) on the 4-D @Interior = {1 <= t <= T-1, 1 <= i, j, k
-- <= N-2}@ domain.  A variant that reads @u[t-1, i-2, j, k]@
-- instead (offset by two) fails the 'IslImageSubsetD' obligation:
-- the image @{(t', i', j, k) : 0 <= t' <= T-2, -1 <= i' <= N-4,
-- 1 <= j, k <= N-2}@ has @i' = -1@ at @i = 1@ for @N >= 3@, which
-- is not in @TimeBox = {0 <= i <= N-1}@.  The plugin's parametric
-- subset check rejects.
--
-- Uses the force-method pattern on a direct 'IslImageSubset' proof.

type BadNeighborIm2 =
  '[ 'TDim (D 0) -. 'TConst ('Pos 1)
   , 'TDim (D 1) -. 'TConst ('Pos 2)
   , 'TDim (D 2)
   , 'TDim (D 3)
   ] :: [TExpr '["N","T"] 4]

proofBadHeat3DOutOfBoundsNeighbor
  :: IslImageSubset '["N","T"] 4 4
       (IslMultiAffToMap '["N","T"] 4 4 BadNeighborIm2)
       Interior
       TimeBox
  => ()
proofBadHeat3DOutOfBoundsNeighbor =
  islImageSubsetEv
    @'["N","T"] @4 @4
    @(IslMultiAffToMap '["N","T"] 4 4 BadNeighborIm2)
    @Interior
    @TimeBox

_useBadHeat3DOutOfBoundsNeighbor :: ()
_useBadHeat3DOutOfBoundsNeighbor = proofBadHeat3DOutOfBoundsNeighbor

forceBadHeat3DOutOfBoundsNeighbor :: () -> IO ()
forceBadHeat3DOutOfBoundsNeighbor () = do
  _ <- evaluate _useBadHeat3DOutOfBoundsNeighbor
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #11: 'IslPartitions' rejects a coverage gap
-- ═══════════════════════════════════════════════════════════════════════
--
-- 'IslPartitions' on 'Alpha.Surface.Core.Case' requires branch domains
-- to cover the ambient AND be pairwise disjoint within it.  This test
-- exercises the coverage half with the same domains as
-- 'forceBadCholeskyCoverageGap' — ambient 'LowerTri', branches just
-- '[StrictLowerN]' missing the diagonal.  Pins that 'IslPartitions'
-- catches pure coverage gaps (not only disjointness violations).

proofBadPartitionsCoverageGap
  :: IslPartitions '["N"] 2 '[LowerTri] '[StrictLowerN] => ()
proofBadPartitionsCoverageGap =
  islPartitionsEv @'["N"] @2 @'[LowerTri] @'[StrictLowerN]

_useBadPartitionsCoverageGap :: ()
_useBadPartitionsCoverageGap = proofBadPartitionsCoverageGap

forceBadCasePartitionsCoverageGap :: () -> IO ()
forceBadCasePartitionsCoverageGap () = do
  _ <- evaluate _useBadPartitionsCoverageGap
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #12: 'IslPartitions' rejects non-disjoint branches
-- ═══════════════════════════════════════════════════════════════════════
--
-- The disjointness half of 'IslPartitions': ambient 'LowerTri', with
-- two branches @DiagN@ and @LowerTri@ itself.  'DiagN ⊆ LowerTri', so
-- the pairwise intersection within the ambient is 'DiagN' — non-empty
-- for any @N ≥ 1@.  A well-formed 'Case' must NEVER admit two
-- branches covering the same point, even if the second branch is
-- strictly wider than the first.  The plugin's 'WantedPartitions'
-- handler reports the offending pair of branches and the overlap set.

proofBadPartitionsNonDisjoint
  :: IslPartitions '["N"] 2 '[LowerTri] '[DiagN, LowerTri] => ()
proofBadPartitionsNonDisjoint =
  islPartitionsEv @'["N"] @2 @'[LowerTri] @'[DiagN, LowerTri]

_useBadPartitionsNonDisjoint :: ()
_useBadPartitionsNonDisjoint = proofBadPartitionsNonDisjoint

forceBadCasePartitionsNonDisjoint :: () -> IO ()
forceBadCasePartitionsNonDisjoint () = do
  _ <- evaluate _useBadPartitionsNonDisjoint
  return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #13: reduction with strictly partial image
-- ═══════════════════════════════════════════════════════════════════════
--
-- 'Alpha.Surface.Core.Reduce' carries an 'IslImageEqualD' obligation:
-- the reduction's projection image must equal its declared ambient.
-- A reduction whose image is a /strict/ subset of the ambient is the
-- genuine partiality bug — a function that masquerades as total but
-- has undefined values on part of its domain.  This proof exercises
-- the rejection.
--
-- Body @PartialBody = {(i, j) : 1 <= i <= N, 0 <= j <= N}@ excludes
-- @i = 0@.  Projection @(i, j) -> (i)@ has image @{i : 1 <= i <= N}@.
-- Target @FullTarget = {i : 0 <= i <= N}@ /includes/ @i = 0@, so the
-- image is a strict subset of the target and 'IslImageEqual' must fail.
--
-- Uses the force-method pattern via 'islImageEqualEv'.  This binding
-- is parked (the whole 'Negative.Cases' module is excluded from the
-- alpha-test suite) because the plugin error path emits hard errors
-- that '-fdefer-type-errors' can't intercept on GHC 9.10.

type PartialBody =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 1)
   , 'TParam (P "N") >=. 'TDim (D 0)
   , 'TDim (D 1)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 1)
   ] :: [TConstraint '["N"] 2]

type FullTarget =
  '[ 'TDim (D 0)     >=. 'TConst ('Pos 0)
   , 'TParam (P "N") >=. 'TDim (D 0)
   ] :: [TConstraint '["N"] 1]

-- Projection map @(i, j) -> (i)@ as a multi-aff.
type ProjectI =
  '[ 'TDim (D 0)
   ] :: [TExpr '["N"] 2]

proofBadReducePartial
  :: IslImageEqual '["N"] 2 1
       (IslMultiAffToMap '["N"] 2 1 ProjectI)
       PartialBody
       FullTarget
  => ()
proofBadReducePartial =
  islImageEqualEv
    @'["N"] @2 @1
    @(IslMultiAffToMap '["N"] 2 1 ProjectI)
    @PartialBody
    @FullTarget

_useBadReducePartial :: ()
_useBadReducePartial = proofBadReducePartial

forceBadReducePartial :: () -> IO ()
forceBadReducePartial () = do
  _ <- evaluate _useBadReducePartial
  return ()
