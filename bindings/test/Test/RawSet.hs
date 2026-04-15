{-# LANGUAGE QualifiedDo #-}
module Test.RawSet (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..), unsafeIslFromIO)
import Isl.Linear (query_, queryM_)
import qualified Isl.Linear as Isl
import qualified Isl.Set as S
import qualified Isl.BasicSet as BS
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.Set"
  [ testCase "fromBasicSet preserves content" $ do
      let r = runIslTest $ Isl.do
            bs <- BS.readFromStr "{ [i] : 0 <= i <= 5 }"
            s <- S.fromBasicSet bs
            Ur e <- query_ s S.isEmpty
            Isl.pure (Ur e)
      assertBool "set should not be empty" (not r)

  , testCase "union of disjoint sets" $ do
      let r = runIslTest $ Isl.do
            s1 <- S.readFromStr "{ [i] : 0 <= i <= 3 }"
            s2 <- S.readFromStr "{ [i] : 7 <= i <= 10 }"
            s3 <- S.union s1 s2
            Ur e <- query_ s3 S.isEmpty
            Isl.pure (Ur e)
      assertBool "union should not be empty" (not r)

  , testCase "subtract self gives empty" $ do
      let r = runIslTest $ Isl.do
            s1 <- S.readFromStr "{ [i] : 0 <= i <= 5 }"
            s2 <- S.readFromStr "{ [i] : 0 <= i <= 5 }"
            s3 <- S.subtract s1 s2
            Ur e <- query_ s3 S.isEmpty
            Isl.pure (Ur e)
      assertBool "s - s should be empty" r

  , testCase "intersect commutes" $ do
      let r = runIslTest $ Isl.do
            s1 <- S.readFromStr "{ [i] : 0 <= i <= 10 }"
            s2 <- S.readFromStr "{ [i] : 5 <= i <= 15 }"
            s1a <- S.readFromStr "{ [i] : 0 <= i <= 10 }"
            s2a <- S.readFromStr "{ [i] : 5 <= i <= 15 }"
            ab <- S.intersect s1 s2
            ba <- S.intersect s2a s1a
            Ur eq <- queryM_ ab (\rAB ->
              query_ ba (\rBA -> S.isEqual rAB rBA))
            Isl.pure (Ur eq)
      assertBool "intersect should commute" r

  , testCase "isSubset" $ do
      let r = runIslTest $ Isl.do
            s1 <- S.readFromStr "{ [i] : 2 <= i <= 5 }"
            s2 <- S.readFromStr "{ [i] : 0 <= i <= 10 }"
            Ur sub <- queryM_ s1 (\r1 ->
              query_ s2 (\r2 -> S.isSubset r1 r2))
            Isl.pure (Ur sub)
      assertBool "[2,5] should be subset of [0,10]" r

  , testCase "complement" $ do
      let r = runIslTest $ Isl.do
            s <- S.readFromStr "[N] -> { [i] : 0 <= i <= N }"
            c <- S.complement s
            Ur e <- query_ c S.isEmpty
            Isl.pure (Ur e)
      assertBool "complement of bounded set should not be empty" (not r)

  , testCase "coalesce" $ do
      let r = runIslTest $ Isl.do
            s <- S.readFromStr "{ [i] : 0 <= i <= 5 or 3 <= i <= 8 }"
            s' <- S.coalesce s
            Ur e <- query_ s' S.isEmpty
            Isl.pure (Ur e)
      assertBool "coalesced set should not be empty" (not r)

  , testCase "foreachBasicSet enumerates disjuncts" $ do
      let r = runIslTest $ Isl.do
            s <- S.readFromStr "{ [i] : 0 <= i <= 3 or 7 <= i <= 10 }"
            Ur count <- queryM_ s (\sRef ->
              unsafeIslFromIO $ \_ -> do
                cs <- S.foreachBasicSet sRef $ \_ -> return ()
                return (Ur (length cs)))
            Isl.pure (Ur count)
      assertBool "should have at least 2 basic sets" (r >= 2)
  ]
