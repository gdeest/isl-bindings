{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Map (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Isl.Monad (Ur(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices

import qualified Isl.HighLevel.BasicMap as BM
import qualified Isl.HighLevel.Map as Map
import qualified Isl.HighLevel.Set as Set

import Test.Generators
import Test.Helpers (runIslTest, mkMap)

tests :: TestTree
tests = testGroup "Map"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit"
  [ testCase "fromString parses correctly" $ do
      let result = runIslTest $ do
            m <- Map.fromString @_ @1 @1 "{ [i] -> [j] : 0 <= i <= 10 and j = i + 1 }"
            (Ur str, m') <- Map.borrowMap m Map.mapToString
            Map.freeMap m'
            return (Ur str)
      assertBool "should produce valid string" (not $ null result)

  , testCase "union of maps" $ do
      let empty = runIslTest $ do
            bm1 <- BM.mkBasicMap @1 @1 $ \(i :- Nil) (j :- Nil) ->
              idx j ==: idx i +: cst 1
              &&: idx i >=: cst 0 &&: idx i <=: cst 5
            bm2 <- BM.mkBasicMap @1 @1 $ \(i :- Nil) (j :- Nil) ->
              idx j ==: idx i +: cst 2
              &&: idx i >=: cst 0 &&: idx i <=: cst 5
            m1 <- Map.fromBasicMap bm1
            m2 <- Map.fromBasicMap bm2
            u <- Map.union m1 m2
            (Ur result, u') <- Map.isEmpty u
            Map.freeMap u'
            return (Ur result)
      assertBool "union of maps should not be empty" (not empty)

  , testCase "subtract self gives empty" $ do
      let empty = runIslTest $ do
            let mk = BM.mkBasicMap @1 @1 $ \(i :- Nil) (j :- Nil) ->
                  idx j ==: idx i +: cst 1
                  &&: idx i >=: cst 0 &&: idx i <=: cst 10
            bm1 <- mk
            bm2 <- mk
            m1 <- Map.fromBasicMap bm1
            m2 <- Map.fromBasicMap bm2
            diff <- Map.subtract m1 m2
            (Ur result, diff') <- Map.isEmpty diff
            Map.freeMap diff'
            return (Ur result)
      assertBool "M \\ M should be empty" empty

  , testCase "domain of identity-like map" $ do
      let result = runIslTest $ do
            bm <- BM.mkBasicMap @1 @1 $ \(i :- Nil) (j :- Nil) ->
              idx i >=: cst 0 &&: idx i <=: cst 10
              &&: idx j ==: idx i
            m <- Map.fromBasicMap bm
            dom <- Map.domain m
            (Ur str, dom') <- Set.borrowSet dom Set.setToString
            Set.freeSet dom'
            return (Ur str)
      assertBool "domain should be non-empty" (not $ null result)
  ]

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ testProperty "union is commutative" $ \(MapConj11 ca) (MapConj11 cb) ->
      runIslTest $ do
        a1 <- mkMap @1 @1 ca; a2 <- mkMap @1 @1 ca
        b1 <- mkMap @1 @1 cb; b2 <- mkMap @1 @1 cb
        lhs <- Map.union a1 b1
        rhs <- Map.union b2 a2
        (Ur eq, lhs', rhs') <- Map.isEqual lhs rhs
        Map.freeMap lhs'; Map.freeMap rhs'
        return (Ur eq)

  , testProperty "intersect is commutative" $ \(MapConj11 ca) (MapConj11 cb) ->
      runIslTest $ do
        a1 <- mkMap @1 @1 ca; a2 <- mkMap @1 @1 ca
        b1 <- mkMap @1 @1 cb; b2 <- mkMap @1 @1 cb
        lhs <- Map.intersect a1 b1
        rhs <- Map.intersect b2 a2
        (Ur eq, lhs', rhs') <- Map.isEqual lhs rhs
        Map.freeMap lhs'; Map.freeMap rhs'
        return (Ur eq)

  , testProperty "subtract self is empty" $ \(MapConj11 ca) ->
      runIslTest $ do
        a1 <- mkMap @1 @1 ca; a2 <- mkMap @1 @1 ca
        diff <- Map.subtract a1 a2
        (Ur result, diff') <- Map.isEmpty diff
        Map.freeMap diff'
        return (Ur result)
  ]
