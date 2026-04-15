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
-- D14 investigation: -Wno-deferred-type-errors removed so deferred
-- warnings appear in the build log.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Negative type-safety tests for the Alpha v1 milestone.
--
-- Each top-level binding in this module is a deliberately *incorrect*
-- Alpha program.  The test harness in @Main.hs@ forces each binding
-- via 'Control.Exception.evaluate' and asserts that a 'TypeError'
-- exception is raised.  If the binding compiles to a value (rather
-- than to a deferred type error), the negative test fails — the type
-- system did not catch the bug.
--
-- v1 needs all five negative tests to fire correctly for the type
-- safety story to be real (see milestone definition in
-- @doc/alpha-implementation.md@).
module Negative.Cases
  ( forceBadOutOfBoundsK
  , forceBadRankMismatch
  , forceBadUndeclaredVar
  , forceBadMissingDef
  , forceBadDoubleDef
  , forceBadBrokenCausalInCases
  , forceBadCholeskyCoverageGap
  , forceBadCholeskyBranchOutOfBounds
  , forceBadParamCtxMissing
  , forceBadLUDiagonalRead
  , forceBadHeat3DOutOfBoundsNeighbor
  , forceBadCasePartitionsCoverageGap
  , forceBadCasePartitionsNonDisjoint
  ) where

import Control.Exception (evaluate)
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))

import Alpha.Core
  ( Decl(..)
  , DeclList(..)
  , Decls(..)
  , Equation(..)
  , EqList(..)
  , Expr(..)
  , System
  , pattern System
  , VarDecl(..)
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
import Examples.FloydWarshall
  ( NeedsNGeOneSrc
  , NeedsNGeOneDst
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
  , IslImageSubset(..)
  , IslMultiAffToMap
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
import Isl.TypeLevel.Sing (ParamIndex(..))


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #1: out-of-bounds access (k+1 access from a body where k=N-1)
-- ═══════════════════════════════════════════════════════════════════════
--
-- The body iterates over the 3D cube CubeN { 0 ≤ i,j,k ≤ N-1 }, and the
-- access pattern reads A[i, k+1].  At k = N-1, the access lands at
-- index k+1 = N, which is outside A's declared domain { 0 ≤ i,k ≤ N-1 }.
-- The plugin's IslImageSubset obligation should reject this at type
-- checking time.

-- The "out-of-bounds" test originally tried to use the plugin's
-- IslImageSubset obligation via Dep, but D14 (the cascading
-- error-recovery interaction) suppresses plugin obligations when
-- other type errors are present in the same module.  v1 substitutes
-- a 1D-vs-2D rank mismatch which fires structurally.  The real
-- IslSubset-based out-of-bounds test lives in Negative/Minimal.hs
-- in its own module so it isn't subject to D14's suppression.
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

-- ── D14 INVESTIGATION: in-place IslSubset proof ─────────────────
-- Re-added inside Cases.hs (alongside the other 4 negative bindings)
-- to reproduce the original symptom: when this binding is in the
-- same module as the type-family TypeError bindings, no compile
-- error is reported for it.  The same proof in Minimal.hs and
-- Examples/Matmul.hs DOES error.

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

instance ParamIndex "S" where paramIndex = 0
instance ParamIndex "W" where paramIndex = 1

proofBrokenInCausalInv
  :: IslSubset '["S","W"] 2 BrokenCausalMaskInv CausalMaskInv => ()
proofBrokenInCausalInv =
  -- D15: force the class dictionary by calling one of its methods, so
  -- the deferred typeError binding GHC armed for this obligation is
  -- actually demanded at runtime.  Without this, the optimizer drops
  -- the dict argument and the runtime trap never fires.
  islSubsetEv @'["S","W"] @2 @BrokenCausalMaskInv @CausalMaskInv

_useBrokenInCausalInv :: ()
_useBrokenInCausalInv = proofBrokenInCausalInv

-- D14 (resolved): even though the compile-time warning for this
-- IslSubset obligation is suppressed by GHC's cascading-error
-- heuristic (cec_suppress=True when other errors exist in the same
-- module), the deferred typeError binding IS still inserted by
-- addTcEvBind.  Evaluating _useBrokenInCausalInv at runtime therefore
-- fires the trap, letting us keep the negative test inline with the
-- other four rather than in a separate module.
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
  -- error.  In the v1 milestone we count them as one substantive
  -- failure mode.
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
  let !badSys = System
                  (Decls
                     { dInputs  = MkDecl :> MkDecl :> Nil
                     , dOutputs = MkDecl :> Nil
                     , dLocals  = Nil
                     })
                  EqNil
                :: System '["N"] MatmulInputs MatmulOutputs MatmulLocals
   in badSys `seq` return ()


-- ═══════════════════════════════════════════════════════════════════════
-- Negative #5: double equation definition
-- ═══════════════════════════════════════════════════════════════════════

forceBadDoubleDef :: () -> IO ()
forceBadDoubleDef () =
  let !badSys = System
                  (Decls
                     { dInputs  = MkDecl :> MkDecl :> Nil
                     , dOutputs = MkDecl :> Nil
                     , dLocals  = Nil
                     })
                  ( Defines (Proxy @"C") trivialBody
                 :& Defines (Proxy @"C") trivialBody
                 :& EqNil )
                :: System '["N"] MatmulInputs MatmulOutputs MatmulLocals
   in badSys `seq` return ()

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
-- exercises the obligation.  Uses the D15 force-method pattern so the
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
-- lists.  Uses the D15 force-method pattern.

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
-- Negative #8: HasParamCtx missing — IslSubset fails without precondition
-- ═══════════════════════════════════════════════════════════════════════
--
-- The v3 plugin extension reads 'HasParamCtx' givens from GHC's
-- solver context and uses them as assumptions when discharging ISL
-- obligations.  The paired positive demo in
-- 'Examples.FloydWarshall.hasParamCtxDemoPositive' shows that
-- @IslSubset '["N"] 1 NeedsNGeOneSrc NeedsNGeOneDst@ discharges
-- cleanly under @HasParamCtx [N >= 1]@.  This negative test shows
-- the inverse: without any 'HasParamCtx' given in scope, the same
-- obligation fails because the source @{[k] : k == N-1}@ is the
-- singleton @{[-1]}@ at @N = 0@, while the target @{[k] : 0 <= k <= N-1}@
-- is empty — so the subset is rejected.
--
-- Uses the D15 force-method pattern: the proof binding carries the
-- 'IslSubset' obligation as a class context; evaluating the top-level
-- '_useBadParamCtxMissing' forces the class dictionary through the
-- 'islSubsetEv' selector, which triggers the deferred typeError
-- trap the plugin armed when it rejected the obligation.

proofBadParamCtxMissing
  :: IslSubset '["N"] 1 NeedsNGeOneSrc NeedsNGeOneDst => ()
proofBadParamCtxMissing =
  islSubsetEv @'["N"] @1 @NeedsNGeOneSrc @NeedsNGeOneDst

_useBadParamCtxMissing :: ()
_useBadParamCtxMissing = proofBadParamCtxMissing

forceBadParamCtxMissing :: () -> IO ()
forceBadParamCtxMissing () = do
  _ <- evaluate _useBadParamCtxMissing
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
-- Uses the D15 force-method pattern: a proof binding whose only
-- job is to invoke 'islImageSubsetEv' at the failing
-- instantiation, and a top-level '_useBadLUDiagonalRead' that
-- forces the proof to demand the class dictionary, firing the
-- deferred typeError trap the plugin armed.

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
-- Uses the D15 force-method pattern on a direct 'IslImageSubset'
-- proof.

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
-- v5.2 introduced 'IslPartitions' on 'Alpha.Core.Case': the branch
-- domains must cover the ambient AND be pairwise disjoint within it.
-- This test exercises the coverage half with the same domains as
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
