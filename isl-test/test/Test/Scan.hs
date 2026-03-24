{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Scan (tests) where

import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction(..), PDisjunction(..))

import qualified Isl.HighLevel.BasicSet as BS

import Isl.Scan
import Isl.Scan.FSM (scanFSM, scanFoldFSM)

import Test.Helpers

-- Shared test domains

rectConj :: PConjunction '[] 2
rectConj = PConjunction $ Conjunction
  [ InequalityConstraint (Ix (SetDim 0))
  , InequalityConstraint (Add (Constant 2) (Mul (-1) (Ix (SetDim 0))))
  , InequalityConstraint (Ix (SetDim 1))
  , InequalityConstraint (Add (Constant 1) (Mul (-1) (Ix (SetDim 1))))
  ]

triConj :: PConjunction '[] 2
triConj = PConjunction $ Conjunction
  [ InequalityConstraint (Ix (SetDim 0))
  , InequalityConstraint (Add (Constant 2) (Mul (-1) (Ix (SetDim 0))))
  , InequalityConstraint (Ix (SetDim 1))
  , InequalityConstraint (Add (Ix (SetDim 0)) (Mul (-1) (Ix (SetDim 1))))
  ]

paramConj :: PConjunction '["N"] 1
paramConj = PConjunction $ Conjunction
  [ InequalityConstraint (Ix (SetDim 0))
  , InequalityConstraint (Add (Ix (SetParam 0)) (Mul (-1) (Ix (SetDim 0))))
  ]

singlePointConj :: PConjunction '[] 2
singlePointConj = PConjunction $ Conjunction
  [ EqualityConstraint (Add (Mul (-1) (Ix (SetDim 0))) (Constant 5))
  , EqualityConstraint (Add (Mul (-1) (Ix (SetDim 1))) (Constant 3))
  ]

emptyConj :: PConjunction '[] 1
emptyConj = PConjunction $ Conjunction
  [ InequalityConstraint (Ix (SetDim 0))
  , InequalityConstraint (Add (Constant (-1)) (Mul (-1) (Ix (SetDim 0))))
  ]

squareConj :: PConjunction '[] 2
squareConj = PConjunction $ Conjunction
  [ InequalityConstraint (Ix (SetDim 0))
  , InequalityConstraint (Add (Constant 2) (Mul (-1) (Ix (SetDim 0))))
  , InequalityConstraint (Ix (SetDim 1))
  , InequalityConstraint (Add (Constant 2) (Mul (-1) (Ix (SetDim 1))))
  ]

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
      let points = sort $ scanPoints (mkScanner (PDisjunction [rectConj])) []
      assertEqual "6 points" (sort [[0,0],[0,1],[1,0],[1,1],[2,0],[2,1]]) points

  , testCase "triangle" $ do
      let points = sort $ scanPoints (mkScanner (PDisjunction [triConj])) []
      assertEqual "6 points" (sort [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]]) points

  , testCase "parametric" $ do
      let s = mkScanner (PDisjunction [paramConj])
      assertEqual "N=3" [[0],[1],[2],[3]] (scanPoints s [3])
      assertEqual "N=0" [[0]] (scanPoints s [0])

  , testCase "single point" $ do
      let points = scanPoints (mkScanner (PDisjunction [singlePointConj])) []
      assertEqual "one point" [[5,3]] points

  , testCase "empty set" $ do
      let points = scanPoints (mkScanner (PDisjunction [emptyConj])) []
      assertEqual "no points" [] points

  , testCase "ISL round-trip" $ do
      let points = runIslTest $ do
            bs <- BS.mkBasicSet @'[] @2 $ \Nil (x :- y :- Nil) ->
              idx x >=: cst 0 &&: idx x <=: cst 2
              &&: idx y >=: cst 0 &&: idx y <=: idx x
            (Ur (PConjunction conj), bs') <- BS.decomposeBS bs
            BS.freeBS bs'
            let scanner = mkScanner (PDisjunction [PConjunction conj])
            return (Ur (sort $ scanPoints scanner []))
      assertEqual "matches" (sort [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]]) points

  , testCase "scanFold count" $ do
      let count = scanFold (mkScanner (PDisjunction [squareConj])) [] (\n _ -> n + 1) (0 :: Int)
      assertEqual "9 points" 9 count
  ]

-- | FSM scanner tests
fsmTests :: TestTree
fsmTests = testGroup "FSM"
  [ testCase "rectangle" $ do
      let nest = mkLoopNest rectConj
          points = sort $ scanFSM nest []
      assertEqual "6 points" (sort [[0,0],[0,1],[1,0],[1,1],[2,0],[2,1]]) points

  , testCase "triangle" $ do
      let nest = mkLoopNest triConj
          points = sort $ scanFSM nest []
      assertEqual "6 points" (sort [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]]) points

  , testCase "parametric" $ do
      let nest = mkLoopNest paramConj
      assertEqual "N=3" [[0],[1],[2],[3]] (scanFSM nest [3])
      assertEqual "N=0" [[0]] (scanFSM nest [0])

  , testCase "single point" $ do
      let points = scanFSM (mkLoopNest singlePointConj) []
      assertEqual "one point" [[5,3]] points

  , testCase "empty set" $ do
      let points = scanFSM (mkLoopNest emptyConj) []
      assertEqual "no points" [] points

  , testCase "scanFoldFSM count" $ do
      let count = scanFoldFSM (mkLoopNest squareConj) [] (\n _ -> n + 1) (0 :: Int)
      assertEqual "9 points" 9 count

  , testCase "FSM produces lexicographic order" $ do
      let nest = mkLoopNest triConj
          points = scanFSM nest []
      assertEqual "lex order" [[0,0],[1,0],[1,1],[2,0],[2,1],[2,2]] points
  ]

-- | Cross-check: nested loops and FSM produce the same points
crossCheckTests :: TestTree
crossCheckTests = testGroup "Cross-check (nested ≡ FSM)"
  [ testCase "rectangle" $ crossCheck rectConj []
  , testCase "triangle" $ crossCheck triConj []
  , testCase "parametric N=5" $ crossCheck paramConj [5]
  , testCase "parametric N=0" $ crossCheck paramConj [0]
  , testCase "single point" $ crossCheck singlePointConj []
  , testCase "empty set" $ crossCheck emptyConj []
  , testCase "3×3 square" $ crossCheck squareConj []
  ]

crossCheck :: PConjunction ps n -> [Integer] -> Assertion
crossCheck conj params = do
  let Scanner [nest] = mkScanner (PDisjunction [conj])
      nested = sort $ scanPoints (Scanner [nest]) params
      fsm    = sort $ scanFSM nest params
  assertEqual "nested and FSM should produce same points" nested fsm
