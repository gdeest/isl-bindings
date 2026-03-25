{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Multi-statement matmul via polyhedral scanning.
--
-- Two statements:
--   S0: C[i][j] = 0                    domain: { S0[i,j] : 0<=i<N, 0<=j<M }
--   S1: C[i][j] += A[i][k] * B[k][j]  domain: { S1[i,j,k] : 0<=i<N, 0<=j<M, 0<=k<K }
--
-- Schedule (user-defined):
--   { S0[i,j]   -> [i, j, 0, 0] ;
--     S1[i,j,k] -> [i, j, 1, k] }
--
-- This schedules init before accumulation for each (i,j):
--   for i: for j: S0(i,j); for k: S1(i,j,k)
module Main where

import Data.Array (Array, array, (!), elems, listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe (unsafeFreeze)

import Isl.HighLevel.Context
import Isl.HighLevel.Pure (NamedSet(..), NamedMap(..))
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.Linear as Isl
import Isl.Scan

main :: IO ()
main = do
  let n = 3; m = 4; k = 2

  -- Matrices
  let matA = listArray ((0,0),(n-1,k-1))
        [ 1, 2
        , 3, 4
        , 5, 6 ] :: Array (Int,Int) Int
      matB = listArray ((0,0),(k-1,m-1))
        [ 7,  8,  9, 10
        , 11, 12, 13, 14 ] :: Array (Int,Int) Int

  -- Build the multi-scanner via ISL
  let (namedSets, namedMaps) = runIsl $ Isl.do
        -- Domain: union of S0 and S1
        dom <- US.fromString $
          "[K,M,N] -> { S0[i,j] : 0 <= i < N and 0 <= j < M; "
          ++ "S1[i,j,k] : 0 <= i < N and 0 <= j < M and 0 <= k < K }"

        -- Schedule
        sched <- UM.fromString $
          "[K,M,N] -> { S0[i,j] -> [i,j,0,0]; S1[i,j,k] -> [i,j,1,k] }"

        -- Apply schedule to get scheduled domain
        (dom', sched') <- Isl.do
          let (d1, d2) = dup dom
          let (s1, s2) = dup sched
          sd <- US.apply d1 s1
          inv <- UM.reverse s2
          consume d2
          Isl.pure (sd, inv)

        -- Decompose both
        (Ur nsets, dom'') <- US.decomposeUnionSetNamed dom'
        US.freeUnionSet dom''
        (Ur nmaps, inv') <- UM.decomposeUnionMapNamed sched'
        UM.freeUnionMap inv'

        Isl.pure (Ur (nsets, nmaps))

  -- Build multi-scanner
  let ms = mkMultiScannerFromNamed namedSets namedMaps

  -- Print the merged loop nest
  putStrLn "=== Merged loop nest ==="
  putStrLn $ prettyMultiScanner ["t0", "t1", "t2", "t3"] ms

  -- Print per-statement info
  putStrLn "=== Named sets from decomposition ==="
  mapM_ (\ns -> putStrLn $ "  " ++ show (nsName ns) ++ " dims=" ++ show (nsNDims ns)
                ++ " params=" ++ show (nsParams ns)) namedSets

  putStrLn "\n=== Named maps (inverses) ==="
  mapM_ (\nm -> putStrLn $ "  " ++ show (nmDomainName nm)
                ++ " in=" ++ show (nmNIn nm) ++ " out=" ++ show (nmNOut nm)) namedMaps

  -- Run the multi-scanner: K=0, M=1, N=2 (alphabetical param order)
  let params = mkVec @3 [fromIntegral k, fromIntegral m, fromIntegral n]

  putStrLn "\n=== Scanning (first 20 points) ==="
  let points = scanMulti ms params
  mapM_ (\pt -> putStrLn $ "  " ++ spStmt pt
                ++ " orig=" ++ show (spOrigCoord pt)
                ++ " time=" ++ show (spTimeCoord pt))
        (take 20 points)
  putStrLn $ "  ... (" ++ show (length points) ++ " total)"

  -- Actually compute the matmul
  matC <- do
    c <- newArray ((0,0),(n-1,m-1)) 0 :: IO (IOUArray (Int,Int) Int)
    scanMultiForM_ ms params $ \pt -> do
      let orig = spOrigCoord pt
      case spStmt pt of
        "S0" -> do
          -- C[i][j] = 0
          let [ii, jj] = map fromIntegral orig
          writeArray c (ii, jj) 0
        "S1" -> do
          -- C[i][j] += A[i][k] * B[k][j]
          let [ii, jj, kk] = map fromIntegral orig
          old <- readArray c (ii, jj)
          writeArray c (ii, jj) (old + matA ! (ii, kk) * matB ! (kk, jj))
        _ -> return ()
    unsafeFreeze c :: IO (Array (Int,Int) Int)

  putStrLn "\nC = A × B:"
  printMatrix matC n m

  -- Verify
  let expected = naiveMatmul matA matB n m k
  putStrLn $ if elems matC == elems expected
    then "\nResult matches naive matmul."
    else "\nMISMATCH!"

  -- Cross-check: linear vs PQ
  let pointsLinear = scanMulti ms params
      pointsPQ     = scanMultiPQ ms params
  putStrLn $ if pointsLinear == pointsPQ
    then "Linear and PQ scanners agree."
    else "LINEAR/PQ MISMATCH!"

printMatrix :: Show a => Array (Int,Int) a -> Int -> Int -> IO ()
printMatrix mat rows cols =
  mapM_ (\i -> do
    let row = [show (mat ! (i,j)) | j <- [0..cols-1]]
    putStrLn $ "  " ++ unwords (map (padLeft 5) row)
  ) [0..rows-1]
  where padLeft w s = replicate (w - length s) ' ' ++ s

naiveMatmul :: Array (Int,Int) Int -> Array (Int,Int) Int -> Int -> Int -> Int -> Array (Int,Int) Int
naiveMatmul a b nn mm kk = array ((0,0),(nn-1,mm-1))
  [((i,j), sum [a!(i,kv) * b!(kv,j) | kv <- [0..kk-1]])
  | i <- [0..nn-1], j <- [0..mm-1]]
