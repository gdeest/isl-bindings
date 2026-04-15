{-# LANGUAGE QualifiedDo #-}
module Test.RawUnionMap (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..), unsafeIslFromIO)
import Isl.Linear (query_, queryM_)
import qualified Isl.Linear as Isl
import qualified Isl.UnionMap as UM
import qualified Isl.UnionSet as US
import qualified Isl.Map as M
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.UnionMap"
  [ testCase "readFromStr" $ do
      let r = runIslTest $ Isl.do
            um <- UM.readFromStr "{ A[i] -> B[i+1]; C[j] -> D[2*j] }"
            Ur e <- query_ um UM.isEmpty
            Isl.pure (Ur e)
      assertBool "union map should not be empty" (not r)

  , testCase "fromMap" $ do
      let r = runIslTest $ Isl.do
            m <- M.readFromStr "{ [i] -> [j] : j = i + 1 }"
            um <- UM.fromMap m
            Ur e <- query_ um UM.isEmpty
            Isl.pure (Ur e)
      assertBool "union map from map should not be empty" (not r)

  , testCase "union" $ do
      let r = runIslTest $ Isl.do
            um1 <- UM.readFromStr "{ A[i] -> B[i] }"
            um2 <- UM.readFromStr "{ C[j] -> D[j] }"
            um3 <- UM.union um1 um2
            Ur s <- query_ um3 UM.toStr
            Isl.pure (Ur s)
      assertBool "union should contain both" (not $ null r)

  , testCase "reverse" $ do
      let r = runIslTest $ Isl.do
            um <- UM.readFromStr "{ A[i] -> B[i+1] }"
            rev <- UM.reverse um
            Ur e <- query_ rev UM.isEmpty
            Isl.pure (Ur e)
      assertBool "reversed union map should not be empty" (not r)

  , testCase "domain" $ do
      let r = runIslTest $ Isl.do
            um <- UM.readFromStr "{ A[i] -> B[i] : 0 <= i <= 5 }"
            dom <- UM.domain um
            Ur e <- query_ dom US.isEmpty
            Isl.pure (Ur e)
      assertBool "domain should not be empty" (not r)

  , testCase "applyRange (composition)" $ do
      let r = runIslTest $ Isl.do
            um1 <- UM.readFromStr "{ A[i] -> B[i+1] }"
            um2 <- UM.readFromStr "{ B[j] -> C[2*j] }"
            um3 <- UM.applyRange um1 um2
            Ur e <- query_ um3 UM.isEmpty
            Isl.pure (Ur e)
      assertBool "composition should not be empty" (not r)

  , testCase "foreachMap enumerates per-space maps" $ do
      let r = runIslTest $ Isl.do
            um <- UM.readFromStr "{ A[i] -> B[i]; C[j] -> D[j] }"
            Ur count <- queryM_ um (\ref ->
              unsafeIslFromIO $ \_ -> do
                ms <- UM.foreachMap ref $ \_ -> return ()
                return (Ur (length ms)))
            Isl.pure (Ur count)
      assertBool "should have 2 maps" (r == 2)
  ]
