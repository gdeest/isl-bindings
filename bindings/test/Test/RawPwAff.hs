{-# LANGUAGE QualifiedDo #-}
module Test.RawPwAff (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.Linear (query_)
import qualified Isl.Linear as Isl
import qualified Isl.PwAff as PA
import qualified Isl.Set as S
import Test.Helpers (runIslTest)

tests :: TestTree
tests = testGroup "Isl.PwAff"
  [ testCase "readFromStr" $ do
      let r = runIslTest $ Isl.do
            pa <- PA.readFromStr "[N] -> { [i] -> [(i)] : i >= 0 and i <= N }"
            Ur s <- query_ pa PA.toStr
            Isl.pure (Ur s)
      assertBool "should parse piecewise aff" (not $ null r)

  , testCase "dimMax returns piecewise aff" $ do
      let r = runIslTest $ Isl.do
            s <- S.readFromStr "[N] -> { [i] : 0 <= i <= N }"
            pa <- S.dimMax s 0
            Ur str <- query_ pa PA.toStr
            Isl.pure (Ur str)
      assertBool "dimMax should produce valid result" (not $ null r)

  , testCase "dimMin returns piecewise aff" $ do
      let r = runIslTest $ Isl.do
            s <- S.readFromStr "[N] -> { [i] : 0 <= i <= N }"
            pa <- S.dimMin s 0
            Ur str <- query_ pa PA.toStr
            Isl.pure (Ur str)
      assertBool "dimMin should produce valid result" (not $ null r)
  ]
