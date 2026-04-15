{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Test harness for the Alpha milestones v1 + v2 (+ v3).
--
-- The implementation log at @doc/alpha-implementation.md@ records the
-- phases; this harness wires them all into a single Tasty test tree.
-- Four phases are covered:
--
--   * Phase A (matmul literal route): the v1 smoke test — matmul
--     compiles to positional 'Alpha.Core' GADTs, every
--     'IslImageSubsetD' / 'IslRangeOfD' obligation is discharged at
--     compile time by the plugin, and the reference interpreter
--     agrees on two known numerical cases.  Until codegen-to-C lands
--     in v6, the existence of the compiled matmul value is itself
--     the type-safety guarantee.
--
--   * Phase A negative type-safety tests: six deliberately wrong
--     programs (rank mismatch, undeclared var, missing def, double
--     def, rank-shaped out-of-bounds, polyhedral subset failure)
--     plus two cholesky-specific negatives (coverage gap, branch
--     out of bounds).  Each 'forceBad…' function forces GHC to
--     solve the bad obligation at runtime via the D15 force-method
--     pattern (or, for the rank/kind-mismatch cases, bang + seq).
--
--   * Phase B (reflected route end-to-end): v1 Phase B demo — build
--     matmul, compute a fresh runtime ISL set via the @islUnion@
--     mirror, install it as a fresh @KnownDom@-bound tag via
--     @reifyDomFromString@, hand it to @replaceInputDomain@ which
--     validates the consequent read obligations at runtime, then
--     verify via @reflectDomString@ that the new tag holds the
--     expected runtime set.  A matching negative demo exercises
--     @replaceInputDomain@'s runtime-failure path.
--
--   * Phase C (cholesky + case path): v2 — first in-package use of
--     'Case' / 'Branches' / 'IslCoversD'.  Structural existence plus
--     two hand-verified numerical cases; the two cholesky negatives
--     above live in Phase A's negative group.
--
-- Deviations from the design are tracked in
-- @doc/alpha-implementation.md@; notably D14 (GHC cascading-error
-- warning suppression) and D15 (empty-class-dict elision of deferred
-- traps) govern how the negative tests fire at runtime.
module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector.Unboxed as V

import Alpha.Compile (validateSchedule, CompileError(..))
import Alpha.Interpret (interpret)
import Alpha.Schedule
import Isl.Typed.Constraints (Expr(..), MapIx(..))

import qualified Examples.Cholesky as Cholesky
import qualified Examples.FloydWarshall as FW
import qualified Examples.Heat3D as H3
import qualified Examples.OverWideCholesky as OWC
import qualified Examples.LU as LU
import qualified Examples.Matmul as Matmul
import qualified Examples.ReflectedMatmul as Refl
import qualified Examples.ReflectedMatmulFails as ReflFail
import qualified Examples.DepReindex as DepReindex
import qualified Examples.TiledZero1D as TiledZero1D
import qualified Examples.TiledConst3D as TiledConst3D
import qualified Negative.Cases as Neg
import qualified Reference.Cholesky as RefChol
import qualified Reference.FloydWarshall as RefFW
import qualified Reference.Heat3D as RefH3
import qualified Reference.LU as RefLU
import qualified Reference.Matmul as Ref


-- | Run an IO action that internally constructs an Alpha term whose
-- deferred type error should fire.  Assert that doing so raises an
-- exception.
--
-- The @() -> IO ()@ wrapping (rather than a bare top-level value
-- forced with @evaluate@) is required so the test harness can force
-- the trap *on demand* — not at module-load time — and so each
-- negative test's exception is isolated from the others via the
-- 'try' below.  Two patterns are used in 'Negative.Cases':
--
--   * Bang-pattern + @seq@ on a deliberately wrong positional-core
--     term (for rank / kind / structural mismatches that GHC can
--     reject as type-family @TypeError@s without involving the
--     plugin).
--   * The D15 force-method pattern: a top-level proof binding with
--     the broken class context in its signature, whose body invokes
--     the class's nullary @()@ witness method (@islSubsetEv@,
--     @islCoversEv@, @islImageSubsetEv@, …) to demand the deferred
--     dictionary so GHC's @-fdefer-type-errors@ trap fires.
--
-- D14 (see the implementation log) is the reason the plugin's
-- compile-time warning can be *silently suppressed* when multiple
-- errors coexist in the same module; the runtime trap is always
-- still armed, which is why this harness can observe the failure
-- even when no warning prints.
shouldFailWithTypeError :: String -> (() -> IO ()) -> Assertion
shouldFailWithTypeError label action = do
  result <- try @SomeException (action ())
  case result of
    Left _  -> return ()
    Right () -> assertFailure $
      label ++ ": expected a deferred type error to fire, but the action ran cleanly"

main :: IO ()
main = defaultMain $ testGroup "alpha-test"
  [ testGroup "phase-A literal route"
      [ testCase "matmul value compiles and exists" $ do
          -- The act of evaluating Matmul.matmul forces the GADT to be
          -- constructed.  If any plugin obligation in the matmul body
          -- failed (out-of-bounds access, mis-shaped projection, etc.),
          -- this would be a compile error and we'd never reach the
          -- test runner.  At runtime there is nothing to "do" — the
          -- check is structural.
          let _ = Matmul.matmul
          assertBool "matmul system constructed" True

      , testCase "reference matmul: 4x4 identity" $ do
          let n = 4
              identity = V.generate (n * n) $ \ij ->
                if ij `div` n == ij `mod` n then 1.0 else 0.0
              result = Ref.referenceMatmul n identity identity
          -- I * I = I
          assertEqual "I*I=I" identity result

      , testCase "reference matmul: 3x3 known case" $ do
          -- A = [[1,2,3],[4,5,6],[7,8,9]]
          -- B = [[1,0,0],[0,1,0],[0,0,1]]  (identity)
          -- A*B = A
          let a = V.fromList [1,2,3,4,5,6,7,8,9 :: Double]
              b = V.fromList [1,0,0,0,1,0,0,0,1 :: Double]
              result = Ref.referenceMatmul 3 a b
          assertEqual "A*I=A" a result
      ]

  , testGroup "phase-A cholesky literal route"
      [ testCase "cholesky value compiles and exists" $ do
          -- Same structural smoke test as matmul: if Case /
          -- BCons / IslCoversD discharge cleanly in the positive
          -- cholesky, we reach this point; if any plugin
          -- obligation failed it would be a compile error.
          let _ = Cholesky.cholesky
          assertBool "cholesky system constructed" True

      , testCase "reference cholesky: 2x2 known case" $ do
          -- A = [[4, 2], [2, 5]], L = [[2, 0], [1, 2]].
          -- Verify L · L^T = A holds by construction via the
          -- well-known 2x2 cholesky factorisation.
          let a      = V.fromList [4, 2, 2, 5 :: Double]
              result = RefChol.referenceCholesky 2 a
              expected = V.fromList [2, 0, 1, 2 :: Double]
          assertEqual "L(2x2)" expected result

      , testCase "reference cholesky: 3x3 known case" $ do
          -- A = [[4, 2, 2], [2, 5, 5], [2, 5, 14]],
          -- L = [[2, 0, 0], [1, 2, 0], [1, 2, 3]].
          let a      = V.fromList [4, 2, 2, 2, 5, 5, 2, 5, 14 :: Double]
              result = RefChol.referenceCholesky 3 a
              expected = V.fromList [2, 0, 0, 1, 2, 0, 1, 2, 3 :: Double]
          assertEqual "L(3x3)" expected result
      ]

  , testGroup "phase-D floyd-warshall literal route (v3)"
      [ testCase "floyd-warshall module type-checks" $ do
          -- First in-package exercise of 'HasParamCtx': the
          -- 'Result[i,j] = D[N-1,i,j]' slice only discharges its
          -- IslImageSubsetD under 'N >= 1', which 'FW.floyd''s type
          -- signature supplies via the marker class.  If the plugin
          -- failed to pick up the given or inject it into the ISL
          -- set build, 'Examples.FloydWarshall' would not compile
          -- and this test binary would not link.
          --
          -- We cannot reference 'FW.floyd' at the value level here
          -- because instantiating it would demand 'HasParamCtx' at
          -- the call site, and 'main :: IO ()' has no way to
          -- propagate the constraint without materialising a dict
          -- via an unsafe trick (which Alpha's zero-unsafeCoerce
          -- discipline forbids).  Instead we reference a type
          -- synonym from the module, which forces the module to be
          -- loaded (and therefore type-checked) without touching
          -- any constraint.
          let _ = Proxy :: Proxy FW.FWCube3D
          assertBool "FloydWarshall module loaded" True

      , testCase "reference floyd-warshall: 4-node graph" $ do
          -- 4-node graph:
          --   0 -> 1 (5), 0 -> 3 (10)
          --   1 -> 2 (3)
          --   2 -> 3 (1)
          -- All-pairs shortest paths:
          --   from 0: [0, 5, 8, 9]  (0 -> 1 -> 2 -> 3 = 9, beats 10)
          --   from 1: [inf, 0, 3, 4]
          --   from 2: [inf, inf, 0, 1]
          --   from 3: [inf, inf, inf, 0]
          let inf = 1 / 0 :: Double
              a   = V.fromList
                [ 0,    5,    inf,  10
                , inf,  0,    3,    inf
                , inf,  inf,  0,    1
                , inf,  inf,  inf,  0
                ]
              expected = V.fromList
                [ 0,    5,    8,    9
                , inf,  0,    3,    4
                , inf,  inf,  0,    1
                , inf,  inf,  inf,  0
                ]
              result = RefFW.referenceFloydWarshall 4 a
          assertEqual "FW(4-node)" expected result
      ]

  , testGroup "phase-E lu decomposition literal route (v4)"
      [ testCase "lu decomp value compiles and exists" $ do
          -- First in-package example with two outputs on distinct
          -- triangular domains and true cross-variable mutual
          -- recursion (L reads U, U reads L).  If any plugin
          -- obligation failed (the DefinesAllExactlyOnce chain over
          -- two outputs, the IslImageSubsetDs for the four cross-
          -- variable reads, or the U[j,j] diagonal read from L's
          -- 2D body), this file would not compile.
          let _ = LU.luDecomp
          assertBool "LU system constructed" True

      , testCase "reference LU: 3x3 known case" $ do
          -- A = [[4, 3, 2], [8, 11, 8], [4, 13, 25]]
          -- L = [[0, 0, 0], [2, 0, 0], [1, 2, 0]]  (unit diagonal
          --                                         implicit)
          -- U = [[4, 3, 2], [0, 5, 4], [0, 0, 15]]
          -- Cross-check: (L + I) · U = A (verified by hand).
          let a = V.fromList [4, 3, 2, 8, 11, 8, 4, 13, 25 :: Double]
              (l, u) = RefLU.referenceLU 3 a
              expectedL = V.fromList [0, 0, 0, 2, 0, 0, 1, 2, 0 :: Double]
              expectedU = V.fromList [4, 3, 2, 0, 5, 4, 0, 0, 15 :: Double]
          assertEqual "L(3x3)" expectedL l
          assertEqual "U(3x3)" expectedU u

      , testCase "reference LU: 2x2 known case" $ do
          -- A = [[2, 1], [4, 3]]
          -- L = [[0, 0], [2, 0]]  (unit diagonal implicit)
          -- U = [[2, 1], [0, 1]]
          -- Cross-check: (L + I) · U = [[2, 1], [4, 3]] ✓
          let a = V.fromList [2, 1, 4, 3 :: Double]
              (l, u) = RefLU.referenceLU 2 a
              expectedL = V.fromList [0, 0, 2, 0 :: Double]
              expectedU = V.fromList [2, 1, 0, 1 :: Double]
          assertEqual "L(2x2)" expectedL l
          assertEqual "U(2x2)" expectedU u
      ]

  , testGroup "phase-F heat3D stencil literal route (v5)"
      [ testCase "heat3D value compiles and exists" $ do
          -- First in-package 4-D example with two parameters
          -- (N and T) and an 8-branch Case split (t=0 initial
          -- condition, six spatial faces at t>=1, plus interior
          -- at t>=1).  If any of the eight branch-subset
          -- obligations, the union IslCovers obligation, or the
          -- seven per-branch Dep image-subset obligations failed,
          -- this file would not compile.
          let _ = H3.heat3D
          assertBool "Heat3D system constructed" True

      , testCase "reference Heat3D: N=3, T=2, u0 uniform 1" $ do
          -- t=0 slice: all 1s (27 entries) — copy of u0.
          -- t=1 slice: all 0s on the boundary (26 entries), the
          -- single interior point (1, 1, 1) is the average of six
          -- neighbours = 6/6 = 1.
          let n = 3
              nt = 2
              u0 = V.replicate (n * n * n) 1.0
              result = RefH3.referenceHeat3D n nt u0
              -- Expected layout: 27 ones at indices 0..26, then 26
              -- zeros at indices 27..53 except index 40 which is
              -- the interior (1, 1, 1) at t=1 = 1*27 + 1*9 + 1*3 + 1.
              expected = V.generate (nt * n * n * n) $ \idx ->
                if idx < 27 then 1.0
                else if idx == 40 then 1.0
                else 0.0
          assertEqual "Heat3D(N=3,T=2,uniform 1)" expected result

      , testCase "reference Heat3D: N=4, T=2, u0 uniform 1" $ do
          -- At N=4 the interior is 2x2x2 = 8 points; each has all
          -- 6 neighbours at t=0 = 1, so every interior point at
          -- t=1 averages to 1.  Boundary at t=1 is 0.
          let n = 4
              nt = 2
              u0 = V.replicate (n * n * n) 1.0
              result = RefH3.referenceHeat3D n nt u0
              -- t=0 slice (indices 0..63): all 1s.
              -- t=1 slice: 1 at the 8 interior points, 0 elsewhere.
              interior4 = [(i, j, k) | i <- [1, 2], j <- [1, 2], k <- [1, 2]]
              expected = V.generate (nt * n * n * n) $ \idx ->
                if idx < 64 then 1.0
                else
                  let rel = idx - 64  -- offset into t=1 slice
                      i = rel `div` (n * n)
                      rem1 = rel `mod` (n * n)
                      j = rem1 `div` n
                      k = rem1 `mod` n
                   in if (i, j, k) `elem` interior4 then 1.0 else 0.0
          assertEqual "Heat3D(N=4,T=2,uniform 1)" expected result
      ]

  , testGroup "phase-F.2 effective-domain rewrite (v5.2)"
      [ -- Positive regression test for the v5.2 effective-domain
        -- rewrite: a cholesky-shaped 'Case' whose diagonal branch
        -- declares 'OverWideDiagN' (off-by-one on the upper bound,
        -- @i <= N@ instead of @i <= N - 1@).  Under the old
        -- 'IslBranchFit' check this was rejected; under the rewrite
        -- the body type is clipped to @OverWideDiagN ∩ LowerTri =
        -- DiagN@ and the branch is accepted.
        testCase "over-wide cholesky diagonal (i <= N) compiles" $ do
          let _ = OWC.overWideCaseExpr
          assertBool "over-wide Case constructed" True
      ]

  , testGroup "phase-A negative type-safety tests"
      [ -- These tests verify that the type system catches eight
        -- different kinds of bad Alpha programs.  Each 'forceBad…'
        -- function in Negative.Cases uses one of two patterns:
        -- a bang-pattern + 'seq' on a deliberately wrong positional
        -- term (for the rank / kind / structural mismatches) or
        -- the D15 force-method pattern (for plugin-dispatched class
        -- obligations like IslSubset / IslCovers / IslImageSubset).
        -- Every 'shouldFailWithTypeError' call below is expected
        -- to succeed, which means the bad program *did* fail to
        -- type-check and the trap fired at runtime.
        testCase "rank mismatch (1D access of 2D variable)" $
          shouldFailWithTypeError "badRankMismatch" Neg.forceBadRankMismatch
      , testCase "undeclared variable Q" $
          shouldFailWithTypeError "badUndeclaredVar" Neg.forceBadUndeclaredVar
      , testCase "missing equation definition for C" $
          shouldFailWithTypeError "badMissingDef" Neg.forceBadMissingDef
      , testCase "double equation definition for C" $
          shouldFailWithTypeError "badDoubleDef" Neg.forceBadDoubleDef
      , testCase "rank-shaped out-of-bounds (1D access of 2D variable)" $
          shouldFailWithTypeError "badOutOfBoundsK" Neg.forceBadOutOfBoundsK
      , -- Real polyhedral subset failure: BrokenCausalMask ⊆ CausalMask
        -- (the FlashAttention pattern).  Two implementation-log
        -- entries converge to make this test work inside the same
        -- module as the five structural negatives:
        --   * D14 — GHC's cascading-error heuristic (cec_suppress=True)
        --     hides the compile-time warning whenever other negatives
        --     in this module have already emitted errors.  That's
        --     why you don't see a warning for this proof even though
        --     the plugin correctly rejects it.  The deferred runtime
        --     typeError binding is still armed by addTcEvBind.
        --   * D15 — the runtime trap is demanded by the force-method
        --     pattern: 'proofBrokenInCausalInv' calls 'islSubsetEv',
        --     forcing the class dictionary to WHNF.  GHC's
        --     -fdefer-type-errors replaced that dict with
        --     'case typeError "…" of {}', so the call throws.
        testCase "polyhedral subset failure (BrokenCausalMask ⊆ CausalMask)" $
          shouldFailWithTypeError "badBrokenCausalInCases" Neg.forceBadBrokenCausalInCases

      , -- Cholesky coverage gap: Case whose branches fail to cover
        -- the full LowerTri ambient domain.  Exercises the plugin's
        -- IslCovers handler and the D15 force-method pattern on the
        -- nullary 'islCoversEv' method.
        testCase "cholesky coverage gap (missing diagonal branch)" $
          shouldFailWithTypeError "badCholeskyCoverageGap" Neg.forceBadCholeskyCoverageGap

      , -- Cholesky branch out of bounds: BCons with a branch domain
        -- (SquareN) that is not a subset of the ambient (LowerTri).
        -- Exercises the per-branch IslSubset obligation.
        testCase "cholesky branch out of bounds (SquareN ⊄ LowerTri)" $
          shouldFailWithTypeError "badCholeskyBranchOutOfBounds" Neg.forceBadCholeskyBranchOutOfBounds

      , -- v3 HasParamCtx path: an IslSubset obligation that needs
        -- @N >= 1@ to discharge.  Without a 'HasParamCtx' given in
        -- scope, the plugin's paramCtx list is empty and the
        -- subset check runs with no precondition.  The source
        -- '{[N-1]}' is non-empty at @N=0@ (it's '{[-1]}') while
        -- the target '{[0..N-1]}' is empty at @N=0@, so the
        -- subset fails.  Pairs with 'hasParamCtxDemoPositive' in
        -- Examples.FloydWarshall (which discharges the *same*
        -- obligation under 'HasParamCtx [N >= 1]') to pin the v3
        -- plugin extension end-to-end.
        testCase "HasParamCtx missing (IslSubset needs N >= 1)" $
          shouldFailWithTypeError "badParamCtxMissing" Neg.forceBadParamCtxMissing

      , -- v4 LU diagonal read: a variant of LU's U equation
        -- reduction body that reads @L[i, i]@ (L's own diagonal)
        -- instead of @L[i, k]@.  L is strict-lower in the Doolittle
        -- convention so the diagonal is outside its declared
        -- domain and the IslImageSubsetD fails.
        testCase "LU diagonal read (L[i,i] when L is strict-lower)" $
          shouldFailWithTypeError "badLUDiagonalRead" Neg.forceBadLUDiagonalRead

      , -- v5 Heat3D out-of-bounds neighbour: a variant of the
        -- interior body that reads @u[t-1, i-2, j, k]@ instead of
        -- @u[t-1, i-1, j, k]@.  The -2 offset gives an image with
        -- @i' = -1@ at @i = 1@ (which is the interior lower bound)
        -- for @N >= 3@; that's outside @TimeBox@'s @i >= 0@.
        testCase "Heat3D out-of-bounds neighbour (u[t-1, i-2, j, k])" $
          shouldFailWithTypeError "badHeat3DOutOfBoundsNeighbor" Neg.forceBadHeat3DOutOfBoundsNeighbor

      , -- v5.2 IslPartitions coverage: a two-branch system where
        -- the diagonal is missing, leaving a gap inside LowerTri.
        -- The new IslPartitions obligation on 'Alpha.Core.Case'
        -- catches pure coverage gaps as well as disjointness
        -- violations.
        testCase "IslPartitions coverage gap (missing diagonal)" $
          shouldFailWithTypeError "badCasePartitionsCoverageGap"
            Neg.forceBadCasePartitionsCoverageGap

      , -- v5.2 IslPartitions disjointness: two branches (DiagN and
        -- LowerTri) where the first is strictly contained in the
        -- second, so the pairwise intersection within the ambient
        -- is non-empty.  Under first-match codegen semantics this
        -- would silently "work"; IslPartitions makes it a compile
        -- error — a point must be defined by exactly one branch.
        testCase "IslPartitions non-disjoint branches (DiagN ⊂ LowerTri)" $
          shouldFailWithTypeError "badCasePartitionsNonDisjoint"
            Neg.forceBadCasePartitionsNonDisjoint
      ]

  , testGroup "phase-B reflected route end-to-end"
      [ -- The headline v1 demo: a transform that exercises every step
        -- of the reflected pipeline (literal KnownDom, mirror function,
        -- reifyDom, reflected KnownDom, reflectDomString) on a real
        -- matmul program.  See Examples.ReflectedMatmul.
        testCase "augment A's domain via islUnion + replaceInputDomain" $
          Refl.runReflectedMatmulPositive

        -- The runtime obligation-failure path: replaceInputDomain
        -- rejects an unsafe narrowing.  See Examples.ReflectedMatmulFails.
      , testCase "narrow A's domain via islIntersect — should fail" $
          ReflFail.runReflectedMatmulNegative
      ]

  , testGroup "phase-H tile and reindex (constructive)"
      [ testCase "tile \"y\" [Just 4] zero1D" $
          TiledZero1D.runTileZero1D

      , testCase "tile domain check — ISL-computed preimage" $
          TiledZero1D.runTileDomainCheck

      , testCase "reindex \"y\" zero1D — explicit multi-aff" $
          TiledZero1D.runReindexZero1D

      , testCase "tile \"w\" [Just 2, Just 2, Just 2] const3D — full" $
          TiledConst3D.runTileConst3DFull

      , testCase "tile \"w\" [Just 2, Nothing, Just 2] const3D — partial" $
          TiledConst3D.runTileConst3DPartial

      , testCase "tile const3D full domain — ISL-computed 6D" $
          TiledConst3D.runTileConst3DFullDomain

      , testCase "tile const3D partial domain — ISL-computed 5D" $
          TiledConst3D.runTileConst3DPartialDomain

      , testCase "reindex with Dep rewriting — skew A, B reads A" $
          DepReindex.runReindexDep
      ]

  , testGroup "phase-I interpreter"
      [ testCase "interpret matmul N=3" $ do
          let n = 3
              a = V.fromList [1,2,3,4,5,6,7,8,9 :: Double]
              b = V.fromList [9,8,7,6,5,4,3,2,1 :: Double]
              expected = Ref.referenceMatmul n a b
          eval <- interpret Matmul.matmul
                    (Map.fromList [("N", n)])
                    (Map.fromList [ ("A", \[i,j] -> a V.! (i*n+j))
                                  , ("B", \[i,j] -> b V.! (i*n+j)) ])
          -- Check all output points
          forM_ [(i,j) | i <- [0..n-1], j <- [0..n-1]] $ \(i,j) -> do
            got <- eval "C" [i, j]
            let exp' = expected V.! (i*n+j)
            assertBool ("C[" ++ show i ++ "," ++ show j ++ "] = "
                        ++ show got ++ " /= " ++ show exp')
                       (abs (got - exp') < 1e-10)

      , testCase "interpret cholesky N=3" $ do
          let n = 3
              a = V.fromList [4, 2, 2, 2, 5, 5, 2, 5, 14 :: Double]
              expected = RefChol.referenceCholesky n a
          eval <- interpret Cholesky.cholesky
                    (Map.fromList [("N", n)])
                    (Map.fromList [("A", \[i,j] -> a V.! (i*n+j))])
          -- L is on lower triangle (j <= i)
          forM_ [(i,j) | i <- [0..n-1], j <- [0..i]] $ \(i,j) -> do
            got <- eval "L" [i, j]
            let exp' = expected V.! (i*n+j)
            assertBool ("L[" ++ show i ++ "," ++ show j ++ "] = "
                        ++ show got ++ " /= " ++ show exp')
                       (abs (got - exp') < 1e-10)

      , testCase "interpret floyd-warshall N=4" $ do
          let n = 4
              inf = 1/0 :: Double
              a = V.fromList
                [ 0,    5,    inf,  10
                , inf,  0,    3,    inf
                , inf,  inf,  0,    1
                , inf,  inf,  inf,  0
                ]
              expected = RefFW.referenceFloydWarshall n a
          eval <- interpret FW.floyd
                    (Map.fromList [("N", n)])
                    (Map.fromList [("A", \[i,j] -> a V.! (i*n+j))])
          forM_ [(i,j) | i <- [0..n-1], j <- [0..n-1]] $ \(i,j) -> do
            got <- eval "Result" [i, j]
            let exp' = expected V.! (i*n+j)
            assertBool ("Result[" ++ show i ++ "," ++ show j ++ "] = "
                        ++ show got ++ " /= " ++ show exp')
                       (got == exp' || abs (got - exp') < 1e-10)

      , testCase "interpret LU N=3" $ do
          let n = 3
              a = V.fromList [4, 3, 2, 8, 11, 8, 4, 13, 25 :: Double]
              (expectedL, expectedU) = RefLU.referenceLU n a
          eval <- interpret LU.luDecomp
                    (Map.fromList [("N", n)])
                    (Map.fromList [("A", \[i,j] -> a V.! (i*n+j))])
          -- L is strict lower (j < i)
          forM_ [(i,j) | i <- [0..n-1], j <- [0..i-1]] $ \(i,j) -> do
            got <- eval "L" [i, j]
            let exp' = expectedL V.! (i*n+j)
            assertBool ("L[" ++ show i ++ "," ++ show j ++ "] = "
                        ++ show got ++ " /= " ++ show exp')
                       (abs (got - exp') < 1e-10)
          -- U is upper triangle with diagonal (i <= j)
          forM_ [(i,j) | i <- [0..n-1], j <- [i..n-1]] $ \(i,j) -> do
            got <- eval "U" [i, j]
            let exp' = expectedU V.! (i*n+j)
            assertBool ("U[" ++ show i ++ "," ++ show j ++ "] = "
                        ++ show got ++ " /= " ++ show exp')
                       (abs (got - exp') < 1e-10)

      , testCase "interpret heat3D N=3, T=2" $ do
          let n = 3
              nt = 2
              u0 = V.replicate (n*n*n) 1.0
              expected = RefH3.referenceHeat3D n nt u0
          eval <- interpret H3.heat3D
                    (Map.fromList [("N", n), ("T", nt)])
                    (Map.fromList [("u0", \[i,j,k] -> u0 V.! (i*n*n + j*n + k))])
          -- Check all output points u[t,i,j,k]
          forM_ [(t,i,j,k) | t <- [0..nt-1], i <- [0..n-1],
                              j <- [0..n-1], k <- [0..n-1]] $ \(t,i,j,k) -> do
            got <- eval "u" [t, i, j, k]
            let exp' = expected V.! (t*n*n*n + i*n*n + j*n + k)
            assertBool ("u[" ++ show [t,i,j,k] ++ "] = "
                        ++ show got ++ " /= " ++ show exp')
                       (abs (got - exp') < 1e-10)
      ]

  , testGroup "phase-J schedule validation"
      [ testCase "matmul identity schedule — valid (typed)" $ do
          let s = scheduling $ do
                sched @"C" @Matmul.MatmulDecls $ \n -> identity n
          result <- validateSchedule Matmul.matmul s
          case result of
            Right () -> pure ()
            Left err -> assertFailure $ "expected valid schedule, got: " ++ show err

      , testCase "heat3D identity schedule — valid (typed)" $ do
          let s = scheduling $ do
                sched @"u" @H3.H3Decls $ \n -> identity n
          result <- validateSchedule H3.heat3D s
          case result of
            Right () -> pure ()
            Left err -> assertFailure $ "expected valid schedule, got: " ++ show err

      , testCase "heat3D reversed-t schedule — INVALID" $ do
          -- Schedule u[t,i,j,k] → [-t,i,j,k] violates the t-1 dep
          let s = scheduling $ do
                sched @"u" @H3.H3Decls $ \_ -> ScheduleDef
                  [ Mul (-1) (Ix (InDim 0))  -- -t
                  , Ix (InDim 1)             -- i
                  , Ix (InDim 2)             -- j
                  , Ix (InDim 3)             -- k
                  ]
          result <- validateSchedule H3.heat3D s
          case result of
            Left (ScheduleViolation _) -> pure ()
            Right () -> assertFailure "expected schedule violation for reversed-t"
            Left other -> assertFailure $ "unexpected error: " ++ show other

      , testCase "cholesky identity schedule — valid (typed)" $ do
          let s = scheduling $ do
                sched @"L" @Cholesky.CholeskyDecls $ \n -> identity n
          result <- validateSchedule Cholesky.cholesky s
          case result of
            Right () -> pure ()
            Left err -> assertFailure $ "expected valid schedule, got: " ++ show err

      , testCase "floyd-warshall phased schedule — valid (typed)" $ do
          let s = scheduling $ do
                sched @"D"      @FW.FWDecls $ \n -> embedAt 0 (identity n)
                sched @"Result" @FW.FWDecls $ \n -> embedAt 1 (identity n)
          result <- validateSchedule FW.floyd s
          case result of
            Right () -> pure ()
            Left err -> assertFailure $ "expected valid schedule, got: " ++ show err
      ]

  ]
