{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Multi (tests) where

import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit

import Isl.Monad (Ur(..))
import Isl.HighLevel.Context
import Isl.HighLevel.Pure
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.Linear as Isl
import Isl.Types (Dupable(..), Consumable(..))
import Isl.Scan

import Test.Helpers

tests :: TestTree
tests = testGroup "Multi-statement"
  [ tupleNameTests
  , multiScannerTests
  , crossCheckTests
  ]

-- | Helper: decompose a union set and return named sets
decomposeNamed :: String -> [NamedSet]
decomposeNamed str = runIslTest $ Isl.do
  us <- US.fromString str
  (Ur nsets, us') <- US.decomposeUnionSetNamed us
  US.freeUnionSet us'
  Isl.pure (Ur nsets)

-- | Helper: decompose a union map and return named maps
decomposeMapNamed :: String -> [NamedMap]
decomposeMapNamed str = runIslTest $ Isl.do
  um <- UM.fromString str
  (Ur nmaps, um') <- UM.decomposeUnionMapNamed um
  UM.freeUnionMap um'
  Isl.pure (Ur nmaps)

-- | Helper: build a multi-scanner from domain + schedule strings
buildMultiScanner :: String -> String -> (MultiScanner ps nTime, [NamedSet], [NamedMap])
buildMultiScanner domStr schedStr = runIslTest $ Isl.do
  dom <- US.fromString domStr
  sched <- UM.fromString schedStr
  let (d1, d2) = dup dom
  let (s1, s2) = dup sched
  scheduledDom <- US.apply d1 s1
  inv <- UM.reverse s2
  consume d2
  (Ur nsets, sd') <- US.decomposeUnionSetNamed scheduledDom
  US.freeUnionSet sd'
  (Ur nmaps, inv') <- UM.decomposeUnionMapNamed inv
  UM.freeUnionMap inv'
  let ms = mkMultiScannerFromNamed nsets nmaps
  Isl.pure (Ur (ms, nsets, nmaps))

tupleNameTests :: TestTree
tupleNameTests = testGroup "Tuple name extraction"
  [ testCase "set tuple names" $ do
      let nsets = decomposeNamed "{ S0[i,j] : 0 <= i <= 2 and 0 <= j <= 2; S1[k] : 0 <= k <= 3 }"
      let names = sort $ map nsName nsets
      assertEqual "two named sets" [Just "S0", Just "S1"] names

  , testCase "dimension counts" $ do
      let nsets = decomposeNamed "{ A[x,y,z] : 0 <= x,y,z <= 1; B[w] : 0 <= w <= 1 }"
      let dims = sort $ map (\ns -> (nsName ns, nsNDims ns)) nsets
      assertEqual "correct dims" [(Just "A", 3), (Just "B", 1)] dims

  , testCase "parameter names" $ do
      let nsets = decomposeNamed "[N,M] -> { S[i] : 0 <= i < N }"
      case nsets of
        [ns] -> assertEqual "params" ["N", "M"] (nsParams ns)
        _    -> assertFailure "expected one named set"

  , testCase "map domain names" $ do
      let nmaps = decomposeMapNamed "{ S0[i,j] -> [i,j,0]; S1[k] -> [k,0,1] }"
      let names = sort $ map nmDomainName nmaps
      assertEqual "two named maps" [Just "S0", Just "S1"] names
  ]

multiScannerTests :: TestTree
multiScannerTests = testGroup "Multi-scanner"
  [ testCase "simple 2-statement" $ do
      -- S0[i] : 0<=i<=1, S1[j] : 0<=j<=1
      -- Schedule: S0[i] -> [i,0], S1[j] -> [j,1]
      -- Expected order: S0(0), S1(0), S0(1), S1(1)
      let (ms, _, _) = buildMultiScanner
            "{ S0[i] : 0 <= i <= 1; S1[j] : 0 <= j <= 1 }"
            "{ S0[i] -> [i, 0]; S1[j] -> [j, 1] }"
          points = scanMulti ms (mkVec @0 [])
          stmts = map (\p -> (spStmt p, spOrigCoord p)) points
      assertEqual "4 points" 4 (length points)
      assertEqual "correct order"
        [("S0", [0]), ("S1", [0]), ("S0", [1]), ("S1", [1])]
        stmts

  , testCase "init + accumulate matmul" $ do
      let (ms, _, _) = buildMultiScanner
            "[K,M,N] -> { S0[i,j] : 0 <= i < N and 0 <= j < M; S1[i,j,k] : 0 <= i < N and 0 <= j < M and 0 <= k < K }"
            "[K,M,N] -> { S0[i,j] -> [i,j,0,0]; S1[i,j,k] -> [i,j,1,k] }"
          -- N=2, M=2, K=2 → params: K=0, M=1, N=2
          params = mkVec @3 [2, 2, 2]
          points = scanMulti ms params
          -- Should be: for each (i,j): S0 then S1*K
          -- Total: 2*2*(1+2) = 12 points
      assertEqual "12 points" 12 (length points)
      -- First point should be S0 at i=0,j=0
      let first = head points
      assertEqual "first is S0" "S0" (spStmt first)
      assertEqual "first orig" [0, 0] (spOrigCoord first)

  , testCase "empty statement" $ do
      -- S0 domain is empty for N=0
      let (ms, _, _) = buildMultiScanner
            "[N] -> { S0[i] : 0 <= i < N; S1[j] : 0 <= j <= 2 }"
            "[N] -> { S0[i] -> [i, 0]; S1[j] -> [j, 1] }"
          params = mkVec @1 [0]  -- N=0, so S0 is empty
          points = scanMulti ms params
      assertEqual "only S1" 3 (length points)
      assertEqual "all S1" True (all (\p -> spStmt p == "S1") points)

  , testCase "single statement" $ do
      let (ms, _, _) = buildMultiScanner
            "{ S[i,j] : 0 <= i <= 1 and 0 <= j <= 1 }"
            "{ S[i,j] -> [i,j] }"
          points = scanMulti ms (mkVec @0 [])
      assertEqual "4 points" 4 (length points)
      assertEqual "correct coords"
        [[0,0],[0,1],[1,0],[1,1]]
        (map spOrigCoord points)
  ]

crossCheckTests :: TestTree
crossCheckTests = testGroup "Cross-check (linear ≡ PQ)"
  [ testCase "2-statement" $ do
      let (ms, _, _) = buildMultiScanner
            "{ S0[i] : 0 <= i <= 2; S1[j] : 0 <= j <= 2 }"
            "{ S0[i] -> [i, 0]; S1[j] -> [j, 1] }"
          params = mkVec @0 []
          linear = scanMulti ms params
          pq     = scanMultiPQ ms params
      assertEqual "same results" linear pq

  , testCase "matmul" $ do
      let (ms, _, _) = buildMultiScanner
            "[K,M,N] -> { S0[i,j] : 0 <= i < N and 0 <= j < M; S1[i,j,k] : 0 <= i < N and 0 <= j < M and 0 <= k < K }"
            "[K,M,N] -> { S0[i,j] -> [i,j,0,0]; S1[i,j,k] -> [i,j,1,k] }"
          params = mkVec @3 [3, 4, 3]
          linear = scanMulti ms params
          pq     = scanMultiPQ ms params
      assertEqual "same results" linear pq

  , testCase "empty" $ do
      let (ms, _, _) = buildMultiScanner
            "[N] -> { S0[i] : 0 <= i < N }"
            "[N] -> { S0[i] -> [i] }"
          params = mkVec @1 [0]
          linear = scanMulti ms params
          pq     = scanMultiPQ ms params
      assertEqual "both empty" linear pq
  ]
