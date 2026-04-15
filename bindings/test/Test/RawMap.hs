{-# LANGUAGE QualifiedDo #-}
module Test.RawMap (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.Linear (query_, queryM_)
import qualified Isl.Linear as Isl
import qualified Isl.Map as M
import qualified Isl.Set as S
import qualified Isl.Space as Space
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.Map"
  [ testCase "readFromStr" $ do
      let r = runIslTest $ Isl.do
            m <- M.readFromStr "{ [i,j] -> [k] : k = i + j }"
            Ur e <- query_ m M.isEmpty
            Isl.pure (Ur e)
      assertBool "map should not be empty" (not r)

  , testCase "union" $ do
      let r = runIslTest $ Isl.do
            m1 <- M.readFromStr "{ [i] -> [j] : j = i }"
            m2 <- M.readFromStr "{ [i] -> [j] : j = i + 1 }"
            m3 <- M.union m1 m2
            Ur e <- query_ m3 M.isEmpty
            Isl.pure (Ur e)
      assertBool "union should not be empty" (not r)

  , testCase "subtract self gives empty" $ do
      let r = runIslTest $ Isl.do
            m1 <- M.readFromStr "{ [i] -> [j] : j = i and 0 <= i <= 5 }"
            m2 <- M.readFromStr "{ [i] -> [j] : j = i and 0 <= i <= 5 }"
            m3 <- M.subtract m1 m2
            Ur e <- query_ m3 M.isEmpty
            Isl.pure (Ur e)
      assertBool "m - m should be empty" r

  , testCase "domain" $ do
      let r = runIslTest $ Isl.do
            m <- M.readFromStr "{ [i] -> [j] : 0 <= i <= 5 and j = i }"
            d <- M.domain m
            Ur e <- query_ d S.isEmpty
            Isl.pure (Ur e)
      assertBool "domain should not be empty" (not r)

  , testCase "range" $ do
      let r = runIslTest $ Isl.do
            m <- M.readFromStr "{ [i] -> [j] : 0 <= i <= 5 and j = 2*i }"
            rng <- M.range m
            Ur s <- query_ rng S.toStr
            Isl.pure (Ur s)
      assertBool "range should be non-trivial" (length r > 3)

  , testCase "reverse swaps domain and range" $ do
      let r = runIslTest $ Isl.do
            m <- M.readFromStr "{ [i] -> [j] : j = i + 1 and 0 <= i <= 5 }"
            rev <- M.reverse m
            Ur s <- query_ rev M.toStr
            Isl.pure (Ur s)
      assertBool "reversed map should be valid" (not $ null r)

  , testCase "apply (set through map)" $ do
      let r = runIslTest $ Isl.do
            s <- S.readFromStr "{ [i] : 0 <= i <= 5 }"
            m <- M.readFromStr "{ [i] -> [j] : j = i + 1 }"
            img <- S.apply s m
            Ur e <- query_ img S.isEmpty
            Isl.pure (Ur e)
      assertBool "image should not be empty" (not r)

  , testCase "applyRange (composition)" $ do
      let r = runIslTest $ Isl.do
            m1 <- M.readFromStr "{ [i] -> [j] : j = i + 1 }"
            m2 <- M.readFromStr "{ [j] -> [k] : k = 2*j }"
            m3 <- M.applyRange m1 m2
            Ur e <- query_ m3 M.isEmpty
            Isl.pure (Ur e)
      assertBool "composition should not be empty" (not r)

  , testCase "isEqual" $ do
      let r = runIslTest $ Isl.do
            m1 <- M.readFromStr "{ [i] -> [j] : j = i + 1 }"
            m2 <- M.readFromStr "{ [i] -> [j] : j = 1 + i }"
            Ur eq <- queryM_ m1 (\r1 ->
              query_ m2 (\r2 -> M.isEqual r1 r2))
            Isl.pure (Ur eq)
      assertBool "i+1 and 1+i should be equal" r

  , testCase "isSubset" $ do
      let r = runIslTest $ Isl.do
            m1 <- M.readFromStr "{ [i] -> [j] : j = i and 2 <= i <= 5 }"
            m2 <- M.readFromStr "{ [i] -> [j] : j = i and 0 <= i <= 10 }"
            Ur sub <- queryM_ m1 (\r1 ->
              query_ m2 (\r2 -> M.isSubset r1 r2))
            Isl.pure (Ur sub)
      assertBool "restricted map should be subset" r

  , testCase "lexGe produces valid lex relation" $ do
      let r = runIslTest $ Isl.do
            sp <- Space.setAlloc 0 2
            m <- M.lexGe sp
            Ur e <- query_ m M.isEmpty
            Isl.pure (Ur e)
      assertBool "lexGe should not be empty" (not r)
  ]
