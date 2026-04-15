{-# LANGUAGE QualifiedDo #-}
module Test.RawMultiAff (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.Linear (query_, queryM_)
import qualified Isl.Linear as Isl
import qualified Isl.MultiAff as MA
import qualified Isl.BasicMap as BM
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.MultiAff"
  [ testCase "readFromStr" $ do
      let r = runIslTest $ Isl.do
            ma <- MA.readFromStr "{ [i,j] -> [i+j, i-j] }"
            Ur s <- query_ ma MA.toStr
            Isl.pure (Ur s)
      assertBool "should parse multi-aff" (not $ null r)

  , testCase "pullbackMultiAff (composition)" $ do
      let r = runIslTest $ Isl.do
            f <- MA.readFromStr "{ [x] -> [2*x] }"
            g <- MA.readFromStr "{ [i] -> [i+1] }"
            fg <- MA.pullbackMultiAff f g  -- f(g(i)) = 2*(i+1)
            Ur s <- query_ fg MA.toStr
            Isl.pure (Ur s)
      assertBool "composed result should be valid" (not $ null r)

  , testCase "toBasicMap (fromMultiAff)" $ do
      let r = runIslTest $ Isl.do
            ma <- MA.readFromStr "{ [i] -> [i+1] }"
            bm <- BM.fromMultiAff ma
            Ur e <- query_ bm BM.isEmpty
            Isl.pure (Ur e)
      assertBool "basic map from multi-aff should not be empty" (not r)

  , testCase "plainIsEqual" $ do
      let r = runIslTest $ Isl.do
            ma1 <- MA.readFromStr "{ [i] -> [i+1] }"
            ma2 <- MA.readFromStr "{ [i] -> [1+i] }"
            Ur eq <- queryM_ ma1 (\r1 ->
              query_ ma2 (\r2 -> MA.plainIsEqual r1 r2))
            Isl.pure (Ur eq)
      assertBool "i+1 and 1+i should be plainly equal" r

  , testCase "floor" $ do
      let r = runIslTest $ Isl.do
            ma <- MA.readFromStr "{ [i] -> [i/3] }"
            fl <- MA.floor ma
            Ur s <- query_ fl MA.toStr
            Isl.pure (Ur s)
      assertBool "floored result should be valid" (not $ null r)
  ]
