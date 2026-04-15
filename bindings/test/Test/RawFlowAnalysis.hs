{-# LANGUAGE QualifiedDo #-}
module Test.RawFlowAnalysis (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.Linear (query_)
import qualified Isl.Linear as Isl
import qualified Isl.FlowAnalysis as FA
import qualified Isl.UnionMap as UM
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.FlowAnalysis"
  [ testCase "computeFlowDeps on 1D stencil" $ do
      let r = runIslTest $ Isl.do
            -- 1D Jacobi: S[t,i] reads A[t-1,i-1] and A[t-1,i+1], writes A[t,i]
            reads_ <- UM.readFromStr
              "{ S[t,i] -> A[t-1,i-1]; S[t,i] -> A[t-1,i+1] }"
            writes <- UM.readFromStr "{ S[t,i] -> A[t,i] }"
            sched  <- UM.readFromStr "{ S[t,i] -> [t,i] }"
            deps <- FA.computeFlowDeps reads_ writes sched
            Ur e <- query_ deps UM.isEmpty
            Isl.pure (Ur e)
      assertBool "Jacobi should have flow deps" (not r)

  , testCase "no deps when no reads" $ do
      let r = runIslTest $ Isl.do
            reads_ <- UM.readFromStr "{ }"
            writes <- UM.readFromStr "{ S[i] -> A[i] : 0 <= i <= 5 }"
            sched  <- UM.readFromStr "{ S[i] -> [i] }"
            deps <- FA.computeFlowDeps reads_ writes sched
            Ur e <- query_ deps UM.isEmpty
            Isl.pure (Ur e)
      assertBool "no reads means no deps" r
  ]
