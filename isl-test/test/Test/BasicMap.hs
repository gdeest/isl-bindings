{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.BasicMap (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Isl.Monad (Ur(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PMapConjunction(..))

import qualified Isl.HighLevel.BasicSet as BS
import qualified Isl.HighLevel.BasicMap as BM
import qualified Isl.HighLevel.Map as Map
import qualified Isl.HighLevel.Set as Set

import Test.Generators
import Test.Helpers

tests :: TestTree
tests = testGroup "BasicMap"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit"
  [ testCase "fromString produces valid map" $ do
      let result = runIslTest $ do
            bm <- BM.fromString @_ @1 @1 "{ [i] -> [j] : j = i + 1 }"
            (Ur str, bm') <- BM.borrowBM bm BM.bmapToString
            BM.freeBM bm'
            return (Ur str)
      assertBool "should produce valid string" (not $ null result)

  , testCase "domain extraction" $ do
      let result = runIslTest $ do
            bm <- BM.mkBasicMap @1 @1 $ \(i :- Nil) (j :- Nil) ->
              idx i >=: cst 0 &&: idx i <=: cst 10
              &&: idx j ==: idx i +: cst 1
            dom <- BM.domain bm
            (Ur str, dom') <- BS.borrowBS dom BS.bsetToString
            BS.freeBS dom'
            return (Ur str)
      assertBool "domain should be non-empty" (not $ null result)

  , testCase "range extraction" $ do
      let result = runIslTest $ do
            bm <- BM.mkBasicMap @1 @1 $ \(i :- Nil) (j :- Nil) ->
              idx i >=: cst 0 &&: idx i <=: cst 10
              &&: idx j ==: idx i +: cst 1
            rng <- BM.range bm
            (Ur str, rng') <- BS.borrowBS rng BS.bsetToString
            BS.freeBS rng'
            return (Ur str)
      assertBool "range should be non-empty" (not $ null result)

  , testCase "isEmpty on empty map" $ do
      let empty = runIslTest $ do
            -- Contradictory: i >= 0 and i <= -1
            bm <- BM.mkBasicMap @1 @1 $ \(i :- Nil) _ ->
              idx i >=: cst 0 &&: idx i <=: cst (-1)
            (Ur result, bm') <- BM.isEmpty bm
            BM.freeBM bm'
            return (Ur result)
      assertBool "contradictory map should be empty" empty
  ]

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ testProperty "intersect is commutative" $ \(MapConj11 ca) (MapConj11 cb) ->
      runIslTest $ do
        a1 <- BM.toBasicMap @_ @1 @1 ca
        a2 <- BM.toBasicMap @_ @1 @1 ca
        b1 <- BM.toBasicMap @_ @1 @1 cb
        b2 <- BM.toBasicMap @_ @1 @1 cb
        lhs <- BM.intersect a1 b1
        rhs <- BM.intersect b2 a2
        m1 <- Map.fromBasicMap lhs
        m2 <- Map.fromBasicMap rhs
        (Ur eq, m1', m2') <- Map.isEqual m1 m2
        Map.freeMap m1'
        Map.freeMap m2'
        return (Ur eq)

  , testProperty "decomposeBM round-trip preserves map" $ \(MapConj11 ca) ->
      runIslTest $ do
        original <- BM.toBasicMap @_ @1 @1 ca
        (Ur (PMapConjunction decomposed), original') <- BM.decomposeBM original
        rebuilt <- BM.toBasicMap @_ @1 @1 decomposed
        m1 <- Map.fromBasicMap original'
        m2 <- Map.fromBasicMap rebuilt
        (Ur eq, m1', m2') <- Map.isEqual m1 m2
        Map.freeMap m1'
        Map.freeMap m2'
        return (Ur eq)
  ]
