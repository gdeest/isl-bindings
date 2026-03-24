{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Set (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Isl.Monad (Ur(..))
import Isl.HighLevel.Constraints

import qualified Isl.HighLevel.BasicSet as BS
import qualified Isl.HighLevel.Set as Set

import Test.Generators
import Test.Helpers (runIslTest, mkSet)

tests :: TestTree
tests = testGroup "Set"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit"
  [ testCase "fromBasicSet preserves content" $ do
      let eq = runIslTest $ do
            bs1 <- BS.toBasicSet @'[] @2 rect
            bs2 <- BS.toBasicSet @'[] @2 rect
            s <- Set.fromBasicSet bs1
            s2 <- Set.fromBasicSet bs2
            (Ur result, s', s2') <- Set.isEqual s s2
            Set.freeSet s'
            Set.freeSet s2'
            return (Ur result)
      assertBool "fromBasicSet should preserve set" eq

  , testCase "fromString parses correctly" $ do
      let result = runIslTest $ do
            s <- Set.fromString @_ @_ @2 "{ [x, y] : 0 <= x <= 10 and 0 <= y <= 10 }"
            (Ur str, s') <- Set.borrowSet s Set.setToString
            Set.freeSet s'
            return (Ur str)
      assertBool "should produce valid string" (not $ null result)

  , testCase "union of disjoint sets is non-empty" $ do
      let empty = runIslTest $ do
            bs1 <- BS.toBasicSet @'[] @1 $ Conjunction
              [ InequalityConstraint (Ix (SetDim 0))
              , InequalityConstraint (Add (Constant 5) (Mul (-1) (Ix (SetDim 0))))
              ]
            bs2 <- BS.toBasicSet @'[] @1 $ Conjunction
              [ InequalityConstraint (Add (Mul (-1) (Constant 10)) (Ix (SetDim 0)))
              , InequalityConstraint (Add (Constant 15) (Mul (-1) (Ix (SetDim 0))))
              ]
            s1 <- Set.fromBasicSet bs1
            s2 <- Set.fromBasicSet bs2
            u <- Set.union s1 s2
            (Ur result, u') <- Set.isEmpty u
            Set.freeSet u'
            return (Ur result)
      assertBool "union should not be empty" (not empty)

  , testCase "subtract self gives empty" $ do
      let empty = runIslTest $ do
            bs1 <- BS.toBasicSet @'[] @2 rect
            bs2 <- BS.toBasicSet @'[] @2 rect
            s1 <- Set.fromBasicSet bs1
            s2 <- Set.fromBasicSet bs2
            diff <- Set.subtract s1 s2
            (Ur result, diff') <- Set.isEmpty diff
            Set.freeSet diff'
            return (Ur result)
      assertBool "A \\ A should be empty" empty

  , testCase "isEmpty on empty set" $ do
      let empty = runIslTest $ do
            -- Contradictory constraints: x >= 0 and x <= -1
            bs <- BS.toBasicSet @'[] @1 $ Conjunction
              [ InequalityConstraint (Ix (SetDim 0))
              , InequalityConstraint (Add (Constant (-1)) (Mul (-1) (Ix (SetDim 0))))
              ]
            s <- Set.fromBasicSet bs
            (Ur result, s') <- Set.isEmpty s
            Set.freeSet s'
            return (Ur result)
      assertBool "contradictory constraints should be empty" empty
  ]
  where
    rect = Conjunction
      [ InequalityConstraint (Ix (SetDim 0))
      , InequalityConstraint (Add (Constant 10) (Mul (-1) (Ix (SetDim 0))))
      , InequalityConstraint (Ix (SetDim 1))
      , InequalityConstraint (Add (Constant 10) (Mul (-1) (Ix (SetDim 1))))
      ]

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ testProperty "union is commutative" $ \(Conj2 ca) (Conj2 cb) ->
      runIslTest $ do
        a1 <- mkSet @'[] @2 ca; a2 <- mkSet @'[] @2 ca
        b1 <- mkSet @'[] @2 cb; b2 <- mkSet @'[] @2 cb
        lhs <- Set.union a1 b1
        rhs <- Set.union b2 a2
        (Ur eq, lhs', rhs') <- Set.isEqual lhs rhs
        Set.freeSet lhs'; Set.freeSet rhs'
        return (Ur eq)

  , testProperty "intersect is commutative" $ \(Conj2 ca) (Conj2 cb) ->
      runIslTest $ do
        a1 <- mkSet @'[] @2 ca; a2 <- mkSet @'[] @2 ca
        b1 <- mkSet @'[] @2 cb; b2 <- mkSet @'[] @2 cb
        lhs <- Set.intersect a1 b1
        rhs <- Set.intersect b2 a2
        (Ur eq, lhs', rhs') <- Set.isEqual lhs rhs
        Set.freeSet lhs'; Set.freeSet rhs'
        return (Ur eq)

  , testProperty "union is idempotent" $ \(Conj2 ca) ->
      runIslTest $ do
        a1 <- mkSet @'[] @2 ca; a2 <- mkSet @'[] @2 ca; a3 <- mkSet @'[] @2 ca
        aa <- Set.union a1 a2
        (Ur eq, aa', a3') <- Set.isEqual aa a3
        Set.freeSet aa'; Set.freeSet a3'
        return (Ur eq)

  , testProperty "intersect is idempotent" $ \(Conj2 ca) ->
      runIslTest $ do
        a1 <- mkSet @'[] @2 ca; a2 <- mkSet @'[] @2 ca; a3 <- mkSet @'[] @2 ca
        aa <- Set.intersect a1 a2
        (Ur eq, aa', a3') <- Set.isEqual aa a3
        Set.freeSet aa'; Set.freeSet a3'
        return (Ur eq)

  , testProperty "subtract self is empty" $ \(Conj2 ca) ->
      runIslTest $ do
        a1 <- mkSet @'[] @2 ca; a2 <- mkSet @'[] @2 ca
        diff <- Set.subtract a1 a2
        (Ur result, diff') <- Set.isEmpty diff
        Set.freeSet diff'
        return (Ur result)

  , testProperty "De Morgan: complement(a ∪ b) = complement(a) ∩ complement(b)" $
      \(Conj2 ca) (Conj2 cb) ->
        runIslTest $ do
          a1 <- mkSet @'[] @2 ca; a2 <- mkSet @'[] @2 ca
          b1 <- mkSet @'[] @2 cb; b2 <- mkSet @'[] @2 cb
          ab <- Set.union a1 b1
          lhs <- Set.complement ab
          ca' <- Set.complement a2
          cb' <- Set.complement b2
          rhs <- Set.intersect ca' cb'
          (Ur eq, lhs', rhs') <- Set.isEqual lhs rhs
          Set.freeSet lhs'; Set.freeSet rhs'
          return (Ur eq)

  , testProperty "subset iff subtract is empty" $ \(Conj2 ca) (Conj2 cb) ->
      runIslTest $ do
        a1 <- mkSet @'[] @2 ca; a2 <- mkSet @'[] @2 ca
        b1 <- mkSet @'[] @2 cb; b2 <- mkSet @'[] @2 cb
        (Ur sub, a1', b1') <- Set.isSubset a1 b1
        diff <- Set.subtract a2 b2
        (Ur diffEmpty, diff') <- Set.isEmpty diff
        Set.freeSet a1'; Set.freeSet b1'; Set.freeSet diff'
        return (Ur (sub == diffEmpty))
  ]
