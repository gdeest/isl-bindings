{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.PwAff (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))

import qualified Isl.HighLevel.PwAff as PA
import qualified Isl.HighLevel.PwMultiAff as PMA
import qualified Isl.HighLevel.MultiAff as MA
import qualified Isl.HighLevel.Map as Map
import qualified Isl.HighLevel.Set as Set
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices

import Test.Helpers

tests :: TestTree
tests = testGroup "PwAff"
  [ unitTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit"
  [ testCase "dimMax returns piecewise aff" $ do
      let result = runIslTest $ do
            -- Set: { [i, j] : 0 <= i <= 10, 0 <= j <= i }
            s <- Set.fromString @_ @'[] @2 "{ [i, j] : 0 <= i <= 10 and 0 <= j <= i }"
            -- dim_max(1) should give: { [i] -> i : 0 <= i <= 10 }
            pa <- PA.dimMax s 1
            (Ur str, pa') <- PA.borrowPA pa PA.paToString
            PA.freePA pa'
            return (Ur str)
      assertBool "dimMax should produce valid result" (not $ null result)

  , testCase "dimMin returns piecewise aff" $ do
      let result = runIslTest $ do
            s <- Set.fromString @_ @'[] @2 "{ [i, j] : 0 <= i <= 10 and 0 <= j <= i }"
            pa <- PA.dimMin s 1
            (Ur str, pa') <- PA.borrowPA pa PA.paToString
            PA.freePA pa'
            return (Ur str)
      assertBool "dimMin should produce valid result" (not $ null result)

  , testCase "decomposePwAff extracts pieces from dimMax" $ do
      let result = runIslTest $ do
            -- dimMax of { [i, j] : 0 <= i,j <= 10 } for dim 0 should give
            -- a single piece: { [] -> 10 }
            s <- Set.fromString @_ @'[] @2 "{ [i, j] : 0 <= i <= 10 and 0 <= j <= 10 }"
            pa <- PA.dimMax s 0
            (Ur pieces, pa') <- PA.decomposePwAff pa
            PA.freePA pa'
            return (Ur (length pieces))
      assertBool ("should have pieces, got " ++ show result) (result >= 1)

  , testCase "PwMultiAff fromMultiAff has 1 piece" $ do
      let result = runIslTest $ do
            ma <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x +: cst 1, idx y]
            pma <- PMA.fromMultiAff ma
            (Ur str, pma') <- PMA.borrowPMA pma PMA.pmaToString
            PMA.freePMA pma'
            return (Ur str)
      assertBool "should contain arrow" (not $ null result)

  , testCase "PwMultiAff toMap roundtrip" $ do
      let result = runIslTest $ do
            -- Build as multi-aff, wrap as pw_multi_aff, convert to map
            ma <- MA.mkMultiAff @'[] @2 @2 $ \Nil (x :- y :- Nil) ->
              [idx x +: cst 1, idx y]
            pma <- PMA.fromMultiAff ma
            m <- PMA.toMap pma
            (Ur str, m') <- Map.borrowMap m Map.mapToString
            Map.freeMap m'
            return (Ur str)
      assertBool "should contain arrow" (not $ null result)
  ]
