{-# LANGUAGE QualifiedDo #-}
module Test.RawBasicMap (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..), unsafeIslFromIO)
import Isl.Linear (query_)
import qualified Isl.Linear as Isl
import qualified Isl.BasicMap as BM
import qualified Isl.BasicSet as BS
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.BasicMap"
  [ testCase "readFromStr produces valid map" $ do
      let r = runIslTest $ Isl.do
            bm <- BM.readFromStr "{ [i] -> [j] : j = i + 1 }"
            Ur e <- query_ bm BM.isEmpty
            Isl.pure (Ur e)
      assertBool "shift map should not be empty" (not r)

  , testCase "empty map from contradictory constraints" $ do
      let r = runIslTest $ Isl.do
            bm <- BM.readFromStr "{ [i] -> [j] : i >= 5 and i <= 3 }"
            Ur e <- query_ bm BM.isEmpty
            Isl.pure (Ur e)
      assertBool "contradictory map should be empty" r

  , testCase "domain extraction" $ do
      let r = runIslTest $ Isl.do
            bm <- BM.readFromStr "{ [i] -> [j] : 0 <= i <= 5 and j = 2*i }"
            dom <- BM.domain bm
            Ur e <- query_ dom BS.isEmpty
            Isl.pure (Ur e)
      assertBool "domain should not be empty" (not r)

  , testCase "intersect" $ do
      let r = runIslTest $ Isl.do
            bm1 <- BM.readFromStr "{ [i] -> [j] : 0 <= i <= 10 and j = i }"
            bm2 <- BM.readFromStr "{ [i] -> [j] : 5 <= i <= 15 and j = i }"
            bm3 <- BM.intersect bm1 bm2
            Ur e <- query_ bm3 BM.isEmpty
            Isl.pure (Ur e)
      assertBool "intersect should not be empty" (not r)

  , testCase "foreachConstraint" $ do
      let r = runIslTest $ Isl.do
            bm <- BM.readFromStr "{ [i] -> [j] : 0 <= i <= 5 and j = i }"
            Ur n <- Isl.queryM_ bm (\bmRef ->
              unsafeIslFromIO $ \_ -> do
                cs <- BM.foreachConstraint bmRef $ \_ -> return ()
                return (Ur (length cs)))
            Isl.pure (Ur n)
      assertBool "should have constraints" (r >= 2)

  , testCase "toStr" $ do
      let r = runIslTest $ Isl.do
            bm <- BM.readFromStr "[N] -> { [i] -> [j] : 0 <= i <= N and j = i }"
            Ur s <- query_ bm BM.toStr
            Isl.pure (Ur s)
      assertBool "should contain ->" ("->" `elem` words r)
  ]
