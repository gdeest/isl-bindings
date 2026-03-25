{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Multi (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.TypeLits (KnownNat)
import Isl.Monad (Ur(..), runIslT)
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM
import Isl.Scan

tests :: TestTree
tests = testGroup "Multi-statement"
  [ multiScannerTests
  , crossCheckTests
  ]

-- | Build a multi-scanner from one named domain + one named schedule.
-- Uses regular IslT (not Isl.do) to avoid linear-type complexity.
applyAndReverse :: NamedSet -> NamedMap -> IO (NamedSet, NamedMap)
applyAndReverse dom sched = runIslT $ do
  -- Apply schedule
  us <- US.toUnionSetFromNamed dom
  um <- UM.toUnionMapFromNamed sched
  scheduled <- UM.applyToSet us um
  Ur ns <- fst <$> US.decomposeUnionSetNamed scheduled
  -- The scheduled set loses its tuple name; re-tag it
  let ns' = [n { nsName = nsName dom } | n <- ns]

  -- Reverse schedule for inverse
  um2 <- UM.toUnionMapFromNamed sched
  inv <- UM.reverse um2
  Ur nm <- fst <$> UM.decomposeUnionMapNamed inv

  return (Ur (head ns', head nm))

-- | Build multi-scanner from multiple (domain, schedule) pairs.
buildMS :: forall nParams. KnownNat nParams
  => [(NamedSet, NamedMap)] -> IO (MultiScanner nParams)
buildMS pairs = do
  results <- mapM (uncurry applyAndReverse) pairs
  let nsets = map fst results
      nmaps = map snd results
  return $ mkMultiScannerFromNamed @nParams nsets nmaps

multiScannerTests :: TestTree
multiScannerTests = testGroup "Multi-scanner"
  [ testCase "simple 2-statement" $ do
      let s0d = mkNamedPConjunction @"S0" @'[] @1 $
                  \Nil (i :- Nil) -> idx i >=: cst 0 &&: idx i <=: cst 1
          s1d = mkNamedPConjunction @"S1" @'[] @1 $
                  \Nil (j :- Nil) -> idx j >=: cst 0 &&: idx j <=: cst 1
          s0s = mkNamedPMapConjunction @"S0" @'[] @1 @2 $
                  \Nil (i :- Nil) (t0 :- t1 :- Nil) ->
                    idx t0 ==: idx i &&: idx t1 ==: cst 0
          s1s = mkNamedPMapConjunction @"S1" @'[] @1 @2 $
                  \Nil (j :- Nil) (t0 :- t1 :- Nil) ->
                    idx t0 ==: idx j &&: idx t1 ==: cst 1

      ms <- buildMS @0 [(s0d, s0s), (s1d, s1s)]
      let points = scanMulti ms (mkVec @0 [])
          stmts = map (\p -> (spStmt p, spOrigCoord p)) points
      assertEqual "4 points" 4 (length points)
      assertEqual "correct order"
        [("S0", [0]), ("S1", [0]), ("S0", [1]), ("S1", [1])]
        stmts

  , testCase "init + accumulate matmul" $ do
      let s0d = mkNamedPConjunction @"S0" @'["K","M","N"] @2 $
                  \(_kp :- mp :- np :- Nil) (i :- j :- Nil) ->
                    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
                    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
          s1d = mkNamedPConjunction @"S1" @'["K","M","N"] @3 $
                  \(kp :- mp :- np :- Nil) (i :- j :- k :- Nil) ->
                    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
                    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
                    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1
          s0s = mkNamedPMapConjunction @"S0" @'["K","M","N"] @2 @4 $
                  \_ (i :- j :- Nil) (t0 :- t1 :- t2 :- t3 :- Nil) ->
                    idx t0 ==: idx i &&: idx t1 ==: idx j
                    &&: idx t2 ==: cst 0 &&: idx t3 ==: cst 0
          s1s = mkNamedPMapConjunction @"S1" @'["K","M","N"] @3 @4 $
                  \_ (i :- j :- k :- Nil) (t0 :- t1 :- t2 :- t3 :- Nil) ->
                    idx t0 ==: idx i &&: idx t1 ==: idx j
                    &&: idx t2 ==: cst 1 &&: idx t3 ==: idx k

      ms <- buildMS @3 [(s0d, s0s), (s1d, s1s)]
      let params = mkVec @3 [2, 2, 2]
          points = scanMulti ms params
      assertEqual "12 points" 12 (length points)
      assertEqual "first is S0" "S0" (spStmt (head points))
      assertEqual "first orig" [0, 0] (spOrigCoord (head points))

  , testCase "single statement" $ do
      let sd = mkNamedPConjunction @"S" @'[] @2 $
                 \Nil (i :- j :- Nil) ->
                   idx i >=: cst 0 &&: idx i <=: cst 1
                   &&: idx j >=: cst 0 &&: idx j <=: cst 1
          ss = mkNamedPMapConjunction @"S" @'[] @2 @2 $
                 \Nil (i :- j :- Nil) (t0 :- t1 :- Nil) ->
                   idx t0 ==: idx i &&: idx t1 ==: idx j

      ms <- buildMS @0 [(sd, ss)]
      let points = scanMulti ms (mkVec @0 [])
      assertEqual "4 points" 4 (length points)
      assertEqual "correct coords"
        [[0,0],[0,1],[1,0],[1,1]]
        (map spOrigCoord points)
  ]

crossCheckTests :: TestTree
crossCheckTests = testGroup "Cross-check (linear == PQ)"
  [ testCase "2-statement" $ do
      let s0d = mkNamedPConjunction @"S0" @'[] @1 $
                  \Nil (i :- Nil) -> idx i >=: cst 0 &&: idx i <=: cst 2
          s1d = mkNamedPConjunction @"S1" @'[] @1 $
                  \Nil (j :- Nil) -> idx j >=: cst 0 &&: idx j <=: cst 2
          s0s = mkNamedPMapConjunction @"S0" @'[] @1 @2 $
                  \Nil (i :- Nil) (t0 :- t1 :- Nil) ->
                    idx t0 ==: idx i &&: idx t1 ==: cst 0
          s1s = mkNamedPMapConjunction @"S1" @'[] @1 @2 $
                  \Nil (j :- Nil) (t0 :- t1 :- Nil) ->
                    idx t0 ==: idx j &&: idx t1 ==: cst 1
      ms <- buildMS @0 [(s0d, s0s), (s1d, s1s)]
      let params = mkVec @0 []
      assertEqual "same results" (scanMulti ms params) (scanMultiPQ ms params)

  , testCase "matmul" $ do
      let s0d = mkNamedPConjunction @"S0" @'["K","M","N"] @2 $
                  \(_kp :- mp :- np :- Nil) (i :- j :- Nil) ->
                    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
                    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
          s1d = mkNamedPConjunction @"S1" @'["K","M","N"] @3 $
                  \(kp :- mp :- np :- Nil) (i :- j :- k :- Nil) ->
                    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
                    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
                    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1
          s0s = mkNamedPMapConjunction @"S0" @'["K","M","N"] @2 @4 $
                  \_ (i :- j :- Nil) (t0 :- t1 :- t2 :- t3 :- Nil) ->
                    idx t0 ==: idx i &&: idx t1 ==: idx j
                    &&: idx t2 ==: cst 0 &&: idx t3 ==: cst 0
          s1s = mkNamedPMapConjunction @"S1" @'["K","M","N"] @3 @4 $
                  \_ (i :- j :- k :- Nil) (t0 :- t1 :- t2 :- t3 :- Nil) ->
                    idx t0 ==: idx i &&: idx t1 ==: idx j
                    &&: idx t2 ==: cst 1 &&: idx t3 ==: idx k
      ms <- buildMS @3 [(s0d, s0s), (s1d, s1s)]
      let params = mkVec @3 [3, 4, 3]
      assertEqual "same results" (scanMulti ms params) (scanMultiPQ ms params)
  ]
