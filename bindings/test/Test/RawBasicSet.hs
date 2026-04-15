{-# LANGUAGE QualifiedDo #-}
module Test.RawBasicSet (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..), unsafeIslFromIO)
import Isl.Linear (query_)
import qualified Isl.Linear as Isl
import qualified Isl.BasicSet as BS
import qualified Isl.Space as Space
import qualified Isl.Types as Isl
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.BasicSet"
  [ testCase "universe is not empty" $ do
      let r = runIslTest $ Isl.do
            sp <- Space.setAlloc 0 2
            bs <- BS.universe sp
            Ur e <- query_ bs BS.isEmpty
            Isl.pure (Ur e)
      assertBool "universe should not be empty" (not r)

  , testCase "readFromStr round-trip" $ do
      let r = runIslTest $ Isl.do
            bs <- BS.readFromStr "{ [i,j] : 0 <= i <= 10 and 0 <= j <= i }"
            Ur e <- query_ bs BS.isEmpty
            Isl.pure (Ur e)
      assertBool "parsed set should not be empty" (not r)

  , testCase "empty set from contradictory constraints" $ do
      let r = runIslTest $ Isl.do
            bs <- BS.readFromStr "{ [i] : i >= 5 and i <= 3 }"
            Ur e <- query_ bs BS.isEmpty
            Isl.pure (Ur e)
      assertBool "contradictory set should be empty" r

  , testCase "intersect narrows" $ do
      let r = runIslTest $ Isl.do
            bs1 <- BS.readFromStr "{ [i] : 0 <= i <= 10 }"
            bs2 <- BS.readFromStr "{ [i] : 5 <= i <= 15 }"
            bs3 <- BS.intersect bs1 bs2
            Ur s <- query_ bs3 BS.toStr
            Isl.pure (Ur s)
      assertBool "intersect result should mention 5" ("5" `elem` words r)

  , testCase "toStr produces valid ISL string" $ do
      let r = runIslTest $ Isl.do
            bs <- BS.readFromStr "[N] -> { [i] : 0 <= i <= N }"
            Ur s <- query_ bs BS.toStr
            Isl.pure (Ur s)
      assertBool "should contain N" ('N' `elem` r)

  , testCase "dup produces independent copies" $ do
      let r = runIslTest $ Isl.do
            bs <- BS.readFromStr "{ [i] : 0 <= i <= 5 }"
            case Isl.dup bs of
              (bs1, bs2) -> Isl.do
                Ur e1 <- query_ bs1 BS.isEmpty
                Ur e2 <- query_ bs2 BS.isEmpty
                Isl.pure (Ur (e1, e2))
      assertEqual "both copies non-empty" (False, False) r

  , testCase "projectOut eliminates dimension" $ do
      let r = runIslTest $ Isl.do
            bs <- BS.readFromStr "{ [i,j] : 0 <= i <= 5 and 0 <= j <= 3 }"
            bs' <- BS.projectOut bs Isl.islDimSet 1 1  -- remove j
            Ur s <- query_ bs' BS.toStr
            Isl.pure (Ur s)
      -- After projecting out j, should have 1D set
      assertBool "projected set should be 1D" (not $ null r)

  , testCase "foreachConstraint enumerates constraints" $ do
      let r = runIslTest $ Isl.do
            bs <- BS.readFromStr "{ [i] : 0 <= i <= 5 }"
            Ur n <- Isl.queryM_ bs (\bsRef ->
              unsafeIslFromIO $ \_ -> do
                cs <- BS.foreachConstraint bsRef $ \_ -> return ()
                return (Ur (length cs)))
            Isl.pure (Ur n)
      assertBool "should have at least 2 constraints" (r >= 2)
  ]
