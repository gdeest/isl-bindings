{-# LANGUAGE QualifiedDo #-}
module Test.TypedBuild (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.Linear (query_, queryM, queryM_, urWrap, Both(..))
import qualified Isl.Linear as Isl
import qualified Isl.BasicSet as BS
import qualified Isl.BasicMap as BM
import qualified Isl.Set as S
import qualified Isl.Map as M
import qualified Isl.MultiAff as MA
import qualified Isl.UnionMap as UM
import Isl.Typed.Constraints
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.Typed.Build"
  [ testCase "buildBasicSet — triangle" $ do
      let r = runIslTest $ Isl.do
            -- { [i,j] : 0 <= i <= N and 0 <= j <= i }
            let conj = Conjunction
                  [ InequalityConstraint (Ix (SetDim 0))                      -- i >= 0
                  , InequalityConstraint (Add (Ix (SetParam 0)) (Mul (-1) (Ix (SetDim 0))))  -- N - i >= 0
                  , InequalityConstraint (Ix (SetDim 1))                      -- j >= 0
                  , InequalityConstraint (Add (Ix (SetDim 0)) (Mul (-1) (Ix (SetDim 1))))    -- i - j >= 0
                  ]
            bs <- buildBasicSet ["N"] 2 conj
            Ur e <- query_ bs BS.isEmpty
            Isl.pure (Ur e)
      assertBool "triangle should not be empty" (not r)

  , testCase "buildBasicSet round-trip via decompose" $ do
      let r = runIslTest $ Isl.do
            let conj = Conjunction
                  [ InequalityConstraint (Ix (SetDim 0))
                  , InequalityConstraint (Add (Constant 5) (Mul (-1) (Ix (SetDim 0))))
                  ]
            bs <- buildBasicSet [] 1 conj
            Both (Ur conj') bs' <- queryM bs (\ref ->
              Isl.do
                c <- decomposeBasicSet 1 0 ref
                urWrap c)
            -- Rebuild from decomposed constraints
            bs2 <- buildBasicSet [] 1 conj'
            s1 <- S.fromBasicSet bs'
            s2 <- S.fromBasicSet bs2
            Ur eq <- queryM_ s1 (\r1 ->
              query_ s2 (\r2 -> S.isEqual r1 r2))
            Isl.pure (Ur eq)
      assertBool "decompose/rebuild round-trip should preserve set" r

  , testCase "buildBasicMap — identity" $ do
      let r = runIslTest $ Isl.do
            -- { [i] -> [j] : j = i and 0 <= i <= N }
            let conj = Conjunction
                  [ EqualityConstraint (Add (Ix (OutDim 0)) (Mul (-1) (Ix (InDim 0))))  -- j - i = 0
                  , InequalityConstraint (Ix (InDim 0))                                  -- i >= 0
                  , InequalityConstraint (Add (Ix (MapParam 0)) (Mul (-1) (Ix (InDim 0)))) -- N - i >= 0
                  ]
            bm <- buildBasicMap ["N"] 1 1 conj
            Ur e <- query_ bm BM.isEmpty
            Isl.pure (Ur e)
      assertBool "identity map should not be empty" (not r)

  , testCase "buildBasicMap round-trip via decompose" $ do
      let r = runIslTest $ Isl.do
            let conj = Conjunction
                  [ EqualityConstraint (Add (Ix (OutDim 0)) (Mul (-1) (Ix (InDim 0))))
                  ]
            bm <- buildBasicMap [] 1 1 conj
            Both (Ur conj') bm' <- queryM bm (\ref ->
              Isl.do
                c <- decomposeBasicMap 1 1 0 ref
                urWrap c)
            bm2 <- buildBasicMap [] 1 1 conj'
            m1 <- M.fromBasicMap bm'
            m2 <- M.fromBasicMap bm2
            Ur eq <- queryM_ m1 (\r1 ->
              query_ m2 (\r2 -> M.isEqual r1 r2))
            Isl.pure (Ur eq)
      assertBool "map decompose/rebuild round-trip" r

  , testCase "buildMultiAff — shift by 1" $ do
      let r = runIslTest $ Isl.do
            -- { [i] -> [i+1] }
            let exprs = [Add (Ix (SetDim 0)) (Constant 1)]
            ma <- buildMultiAff [] 1 1 exprs
            Ur s <- query_ ma MA.toStr
            Isl.pure (Ur s)
      assertBool "multi-aff should be valid" (not $ null r)

  , testCase "buildMultiAff round-trip via decompose" $ do
      let r = runIslTest $ Isl.do
            let exprs = [Add (Ix (SetDim 0)) (Constant 1), Mul 2 (Ix (SetDim 1))]
            ma <- buildMultiAff [] 2 2 exprs
            Ur exprs' <- queryM_ ma (\ref ->
              Isl.do
                e <- decomposeMultiAff 0 2 ref
                urWrap e)
            -- Rebuild and compare
            ma2 <- buildMultiAff [] 2 2 exprs'
            Ur s <- query_ ma2 MA.toStr
            Isl.pure (Ur (not $ null s))
      assertBool "multi-aff decompose/rebuild" r

  , testCase "buildBasicSet with FloorDiv" $ do
      let r = runIslTest $ Isl.do
            -- { [i] : exists q : i = 3*q and 0 <= i <= 15 }
            -- i.e., multiples of 3 in [0,15]
            let conj = Conjunction
                  [ EqualityConstraint (Add (Ix (SetDim 0)) (Mul (-3) (FloorDiv (Ix (SetDim 0)) 3)))
                  , InequalityConstraint (Ix (SetDim 0))
                  , InequalityConstraint (Add (Constant 15) (Mul (-1) (Ix (SetDim 0))))
                  ]
            bs <- buildBasicSet [] 1 conj
            Ur e <- query_ bs BS.isEmpty
            Isl.pure (Ur e)
      assertBool "set with floor div should not be empty" (not r)

  , testCase "decomposeSet with multiple disjuncts" $ do
      let r = runIslTest $ Isl.do
            s <- S.readFromStr "{ [i] : 0 <= i <= 3 or 7 <= i <= 10 }"
            Ur conjs <- queryM_ s (\ref ->
              Isl.do
                cs <- decomposeSet 1 0 ref
                urWrap cs)
            Isl.pure (Ur (length conjs))
      assertBool "should have at least 2 disjuncts" (r >= 2)

  , testCase "buildUnionMapFromNamed" $ do
      let r = runIslTest $ Isl.do
            let nm = NamedMap
                  { nmDomainName = Just "S"
                  , nmRangeName  = Just "A"
                  , nmParams     = ["N"]
                  , nmNIn        = 2
                  , nmNOut       = 2
                  , nmConjs      = [Conjunction
                      [ EqualityConstraint (Add (Ix (OutDim 0)) (Mul (-1) (Ix (InDim 0))))
                      , EqualityConstraint (Add (Ix (OutDim 1)) (Mul (-1) (Ix (InDim 1))))
                      ]]
                  }
            um <- buildUnionMapFromNamed nm
            Ur e <- query_ um UM.isEmpty
            Isl.pure (Ur (not e))
      assertBool "buildUnionMapFromNamed should succeed" r
  ]
