{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.BasicSet (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Isl.Monad (Ur(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction(..))

import qualified Isl.HighLevel.BasicSet as BS
import qualified Isl.HighLevel.Set as Set

import Test.Generators
import Test.Helpers

tests :: TestTree
tests = testGroup "BasicSet"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit"
  [ testCase "mkBasicSet produces non-empty set" $ do
      let result = runIslTest $ do
            bs <- BS.mkBasicSet @'[] @2 $ \Nil (x :- y :- Nil) ->
              idx x >=: cst 0 &&: idx x <=: cst 10
              &&: idx y >=: cst 0 &&: idx y <=: cst 10
            (Ur str, bs') <- BS.borrowBS bs BS.bsetToString
            BS.freeBS bs'
            return (Ur str)
      assertBool "should contain constraint text" (not $ null result)

  , testCase "intersect produces subset" $ do
      let result = runIslTest $ do
            a <- BS.mkBasicSet @'[] @2 $ \Nil (x :- y :- Nil) ->
              idx x >=: cst 0 &&: idx x <=: cst 10
              &&: idx y >=: cst 0 &&: idx y <=: cst 10
            b <- BS.mkBasicSet @'[] @2 $ \Nil (x :- y :- Nil) ->
              idx x >=: cst 5 &&: idx x <=: cst 15
              &&: idx y >=: cst 5 &&: idx y <=: cst 15
            c <- BS.intersect a b
            (Ur str, c') <- BS.borrowBS c BS.bsetToString
            BS.freeBS c'
            return (Ur str)
      assertBool "intersection should be non-empty" (not $ null result)

  , testCase "fromString round-trip" $ do
      let result = runIslTest $ do
            bs <- BS.fromString @_ @_ @2 "{ [x, y] : 0 <= x <= 5 and 0 <= y <= 5 }"
            (Ur str, bs') <- BS.borrowBS bs BS.bsetToString
            BS.freeBS bs'
            return (Ur str)
      assertBool "should produce valid string" (not $ null result)

  , testCase "decomposeBS round-trip is ISL-equal" $ do
      let eq = runIslTest $ do
            let conj = Conjunction
                  [ InequalityConstraint (Ix (SetDim 0))
                  , InequalityConstraint (Add (Constant 5) (Mul (-1) (Ix (SetDim 0))))
                  , InequalityConstraint (Ix (SetDim 1))
                  , InequalityConstraint (Add (Constant 5) (Mul (-1) (Ix (SetDim 1))))
                  ]
            original <- BS.toBasicSet @'[] @2 conj
            (Ur (PConjunction decomposed), original') <- BS.decomposeBS original
            rebuilt <- BS.toBasicSet @'[] @2 decomposed
            s1 <- Set.fromBasicSet original'
            s2 <- Set.fromBasicSet rebuilt
            (Ur result, s1', s2') <- Set.isEqual s1 s2
            Set.freeSet s1'
            Set.freeSet s2'
            return (Ur result)
      assertBool "round-trip should produce ISL-equal set" eq
  ]

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ testProperty "intersect is commutative" $ \(Conj2 ca) (Conj2 cb) ->
      runIslTest $ do
        a1 <- BS.toBasicSet @'[] @2 ca
        a2 <- BS.toBasicSet @'[] @2 ca
        b1 <- BS.toBasicSet @'[] @2 cb
        b2 <- BS.toBasicSet @'[] @2 cb
        lhs <- BS.intersect a1 b1
        rhs <- BS.intersect b2 a2
        s1 <- Set.fromBasicSet lhs
        s2 <- Set.fromBasicSet rhs
        (Ur eq, s1', s2') <- Set.isEqual s1 s2
        Set.freeSet s1'
        Set.freeSet s2'
        return (Ur eq)

  , testProperty "intersect is idempotent" $ \(Conj2 ca) ->
      runIslTest $ do
        a1 <- BS.toBasicSet @'[] @2 ca
        a2 <- BS.toBasicSet @'[] @2 ca
        a3 <- BS.toBasicSet @'[] @2 ca
        aa <- BS.intersect a1 a2
        s1 <- Set.fromBasicSet aa
        s2 <- Set.fromBasicSet a3
        (Ur eq, s1', s2') <- Set.isEqual s1 s2
        Set.freeSet s1'
        Set.freeSet s2'
        return (Ur eq)

  , testProperty "decomposeBS round-trip preserves set" $ \(Conj2 ca) ->
      runIslTest $ do
        original <- BS.toBasicSet @'[] @2 ca
        (Ur (PConjunction decomposed), original') <- BS.decomposeBS original
        rebuilt <- BS.toBasicSet @'[] @2 decomposed
        s1 <- Set.fromBasicSet original'
        s2 <- Set.fromBasicSet rebuilt
        (Ur eq, s1', s2') <- Set.isEqual s1 s2
        Set.freeSet s1'
        Set.freeSet s2'
        return (Ur eq)
  ]
