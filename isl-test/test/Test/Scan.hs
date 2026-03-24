{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Scan (tests) where

import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import GHC.TypeLits (KnownNat)
import Isl.HighLevel.Params (Length)
import Isl.HighLevel.Pure

import qualified Isl.HighLevel.BasicSet as BS

import Isl.Scan

import Test.Helpers

-- Shared test domains

rectConj :: PConjunction '[] 2
rectConj = mkPConjunction @'[] @2 $ \Nil (x :- y :- Nil) ->
  idx x >=: cst 0 &&: idx x <=: cst 2
  &&: idx y >=: cst 0 &&: idx y <=: cst 1

triConj :: PConjunction '[] 2
triConj = mkPConjunction @'[] @2 $ \Nil (x :- y :- Nil) ->
  idx x >=: cst 0 &&: idx x <=: cst 2
  &&: idx y >=: cst 0 &&: idx y <=: idx x

paramConj :: PConjunction '["N"] 1
paramConj = mkPConjunction @'["N"] @1 $ \(n :- Nil) (x :- Nil) ->
  idx x >=: cst 0 &&: idx x <=: idx n

singlePointConj :: PConjunction '[] 2
singlePointConj = mkPConjunction @'[] @2 $ \Nil (x :- y :- Nil) ->
  idx x ==: cst 5 &&: idx y ==: cst 3

emptyConj :: PConjunction '[] 1
emptyConj = mkPConjunction @'[] @1 $ \Nil (x :- Nil) ->
  idx x >=: cst 0 &&: idx x <=: cst (-1)

squareConj :: PConjunction '[] 2
squareConj = mkPConjunction @'[] @2 $ \Nil (x :- y :- Nil) ->
  idx x >=: cst 0 &&: idx x <=: cst 2
  &&: idx y >=: cst 0 &&: idx y <=: cst 2

tests :: TestTree
tests = testGroup "Scan"
  [ nestedLoopTests
  , fsmTests
  , crossCheckTests
  ]

-- | Nested-loop scanner tests
nestedLoopTests :: TestTree
nestedLoopTests = testGroup "Nested loops"
  [ testCase "rectangle" $ do
      let points = sort $ map toList $ scanPoints (mkScanner (PDisjunction [rectConj])) (mkVec @0 [])
      assertEqual "6 points" (sort [[0,0],[0,1],[1,0],[1,1],[2,0],[2,1]]) points

  , testCase "triangle" $ do
      let points = sort $ map toList $ scanPoints (mkScanner (PDisjunction [triConj])) (mkVec @0 [])
      assertEqual "6 points" (sort [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]]) points

  , testCase "parametric" $ do
      let s = mkScanner (PDisjunction [paramConj])
      assertEqual "N=3" [[0],[1],[2],[3]] (map toList $ scanPoints s (mkVec @1 [3]))
      assertEqual "N=0" [[0]] (map toList $ scanPoints s (mkVec @1 [0]))

  , testCase "single point" $ do
      let points = map toList $ scanPoints (mkScanner (PDisjunction [singlePointConj])) (mkVec @0 [])
      assertEqual "one point" [[5,3]] points

  , testCase "empty set" $ do
      let points = map toList $ scanPoints (mkScanner (PDisjunction [emptyConj])) (mkVec @0 [])
      assertEqual "no points" [] points

  , testCase "ISL round-trip" $ do
      let points = runIslTest $ do
            bs <- BS.mkBasicSet @'[] @2 $ \Nil (x :- y :- Nil) ->
              idx x >=: cst 0 &&: idx x <=: cst 2
              &&: idx y >=: cst 0 &&: idx y <=: idx x
            (Ur (PConjunction conj), bs') <- BS.decomposeBS bs
            BS.freeBS bs'
            let scanner = mkScanner @'[] @2 (PDisjunction [PConjunction conj])
            return (Ur (sort $ map toList $ scanPoints scanner (mkVec @0 [])))
      assertEqual "matches" (sort [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]]) points

  , testCase "scanFold count" $ do
      let count = scanFold (mkScanner (PDisjunction [squareConj])) (mkVec @0 []) (\n _ -> n + 1) (0 :: Int)
      assertEqual "9 points" 9 count
  ]

-- | FSM scanner tests
fsmTests :: TestTree
fsmTests = testGroup "FSM"
  [ testCase "rectangle" $ do
      let nest = mkLoopNest rectConj
          points = sort $ map toList $ scanFSM nest (mkVec @0 [])
      assertEqual "6 points" (sort [[0,0],[0,1],[1,0],[1,1],[2,0],[2,1]]) points

  , testCase "triangle" $ do
      let nest = mkLoopNest triConj
          points = sort $ map toList $ scanFSM nest (mkVec @0 [])
      assertEqual "6 points" (sort [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]]) points

  , testCase "parametric" $ do
      let nest = mkLoopNest paramConj
      assertEqual "N=3" [[0],[1],[2],[3]] (map toList $ scanFSM nest (mkVec @1 [3]))
      assertEqual "N=0" [[0]] (map toList $ scanFSM nest (mkVec @1 [0]))

  , testCase "single point" $ do
      let points = map toList $ scanFSM (mkLoopNest singlePointConj) (mkVec @0 [])
      assertEqual "one point" [[5,3]] points

  , testCase "empty set" $ do
      let points = map toList $ scanFSM (mkLoopNest emptyConj) (mkVec @0 [])
      assertEqual "no points" [] points

  , testCase "scanFoldFSM count" $ do
      let count = scanFoldFSM (mkLoopNest squareConj) (mkVec @0 []) (\n _ -> n + 1) (0 :: Int)
      assertEqual "9 points" 9 count

  , testCase "FSM produces lexicographic order" $ do
      let nest = mkLoopNest triConj
          points = map toList $ scanFSM nest (mkVec @0 [])
      assertEqual "lex order" [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]] points
  ]

-- | Cross-check: nested loops and FSM produce the same points
crossCheckTests :: TestTree
crossCheckTests = testGroup "Cross-check (nested ≡ FSM)"
  [ testCase "rectangle" $ crossCheck rectConj (mkVec @0 [])
  , testCase "triangle" $ crossCheck triConj (mkVec @0 [])
  , testCase "parametric N=5" $ crossCheck paramConj (mkVec @1 [5])
  , testCase "parametric N=0" $ crossCheck paramConj (mkVec @1 [0])
  , testCase "single point" $ crossCheck singlePointConj (mkVec @0 [])
  , testCase "empty set" $ crossCheck emptyConj (mkVec @0 [])
  , testCase "3×3 square" $ crossCheck squareConj (mkVec @0 [])
  ]

crossCheck :: KnownNat (Length ps) => PConjunction ps n -> Vec (Length ps) Integer -> Assertion
crossCheck conj params = do
  let Scanner [nest] = mkScanner (PDisjunction [conj])
      nested = sort $ map toList $ scanPoints (Scanner [nest]) params
      fsm    = sort $ map toList $ scanFSM nest params
  assertEqual "nested and FSM should produce same points" nested fsm
