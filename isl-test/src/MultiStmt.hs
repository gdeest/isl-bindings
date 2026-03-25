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
--   S0[i,j]   -> [i, j, 0, 0]
--   S1[i,j,k] -> [i, j, 1, k]
module Main where

import Data.Array (Array, array, (!), elems, listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe (unsafeFreeze)

import Isl.HighLevel.Context
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure
import qualified Isl.HighLevel.UnionSet as US
import qualified Isl.HighLevel.UnionMap as UM
import qualified Isl.Linear as Isl
import Isl.Scan

-- Pure domain definitions

s0Domain :: NamedSet
s0Domain = mkNamedPConjunction @"S0" @'["K","M","N"] @2 $
  \(_kp :- mp :- np :- Nil) (i :- j :- Nil) ->
    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1

s1Domain :: NamedSet
s1Domain = mkNamedPConjunction @"S1" @'["K","M","N"] @3 $
  \(kp :- mp :- np :- Nil) (i :- j :- k :- Nil) ->
    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1

-- Pure schedule definitions

s0Schedule :: NamedMap
s0Schedule = mkNamedPMapConjunction @"S0" @'["K","M","N"] @2 @4 $
  \_ (i :- j :- Nil) (t0 :- t1 :- t2 :- t3 :- Nil) ->
    idx t0 ==: idx i &&: idx t1 ==: idx j
    &&: idx t2 ==: cst 0 &&: idx t3 ==: cst 0

s1Schedule :: NamedMap
s1Schedule = mkNamedPMapConjunction @"S1" @'["K","M","N"] @3 @4 $
  \_ (i :- j :- k :- Nil) (t0 :- t1 :- t2 :- t3 :- Nil) ->
    idx t0 ==: idx i &&: idx t1 ==: idx j
    &&: idx t2 ==: cst 1 &&: idx t3 ==: idx k

main :: IO ()
main = do
  let n = 3; m = 4; k = 2

  -- Matrices
  let matA = listArray ((0,0),(n-1,k-1))
        [ 1, 2, 3, 4, 5, 6 ] :: Array (Int,Int) Int
      matB = listArray ((0,0),(k-1,m-1))
        [ 7, 8, 9, 10, 11, 12, 13, 14 ] :: Array (Int,Int) Int

  -- Build multi-scanner: lift pure defs to ISL, apply schedule, decompose
  let (scheduledSets, inverseMaps) = runIsl $ Isl.do
        -- Apply schedule to S0's domain
        s0us <- US.toUnionSetFromNamed s0Domain
        s0um <- UM.toUnionMapFromNamed s0Schedule
        s0time <- UM.applyToSet s0us s0um
        (Ur ns0, s0t') <- US.decomposeUnionSetNamed s0time
        US.freeUnionSet s0t'

        -- Apply schedule to S1's domain
        s1us <- US.toUnionSetFromNamed s1Domain
        s1um <- UM.toUnionMapFromNamed s1Schedule
        s1time <- UM.applyToSet s1us s1um
        (Ur ns1, s1t') <- US.decomposeUnionSetNamed s1time
        US.freeUnionSet s1t'

        -- Reverse schedules for inverses
        s0um2 <- UM.toUnionMapFromNamed s0Schedule
        s0inv <- UM.reverse s0um2
        (Ur nm0, s0i') <- UM.decomposeUnionMapNamed s0inv
        UM.freeUnionMap s0i'

        s1um2 <- UM.toUnionMapFromNamed s1Schedule
        s1inv <- UM.reverse s1um2
        (Ur nm1, s1i') <- UM.decomposeUnionMapNamed s1inv
        UM.freeUnionMap s1i'

        -- Tag the scheduled sets with statement names
        -- (ISL apply strips tuple names from the range)
        let tag name nsl = [ns { nsName = Just name } | ns <- nsl]

        Isl.pure (Ur (tag "S0" ns0 ++ tag "S1" ns1, nm0 ++ nm1))

  let ms = mkMultiScannerFromNamed @3 scheduledSets inverseMaps

  -- Print merged loop nest
  putStrLn "=== Merged loop nest ==="
  putStrLn $ prettyMultiScanner ["t0", "t1", "t2", "t3"] ms

  -- Parameters: K=0, M=1, N=2 (alphabetical)
  let params = mkVec @3 [fromIntegral k, fromIntegral m, fromIntegral n]

  putStrLn "=== Scanning (first 20 points) ==="
  let points = scanMulti ms params
  mapM_ (\pt -> putStrLn $ "  " ++ spStmt pt
                ++ " orig=" ++ show (spOrigCoord pt)
                ++ " time=" ++ show (spTimeCoord pt))
        (take 20 points)
  putStrLn $ "  ... (" ++ show (length points) ++ " total)"

  -- Compute matmul
  matC <- do
    c <- newArray ((0,0),(n-1,m-1)) 0 :: IO (IOUArray (Int,Int) Int)
    scanMultiForM_ ms params $ \pt ->
      case spStmt pt of
        "S0" -> do
          let [ii, jj] = map fromIntegral (spOrigCoord pt)
          writeArray c (ii, jj) 0
        "S1" -> do
          let [ii, jj, kk] = map fromIntegral (spOrigCoord pt)
          old <- readArray c (ii, jj)
          writeArray c (ii, jj) (old + matA ! (ii, kk) * matB ! (kk, jj))
        _ -> return ()
    unsafeFreeze c :: IO (Array (Int,Int) Int)

  putStrLn "\nC = A * B:"
  printMatrix matC n m

  let expected = naiveMatmul matA matB n m k
  putStrLn $ if elems matC == elems expected
    then "\nResult matches naive matmul."
    else "\nMISMATCH!"

  -- Cross-check
  putStrLn $ if scanMulti ms params == scanMultiPQ ms params
    then "Linear and PQ scanners agree."
    else "LINEAR/PQ MISMATCH!"

printMatrix :: Show a => Array (Int,Int) a -> Int -> Int -> IO ()
printMatrix mat rows cols =
  mapM_ (\i -> putStrLn $ "  " ++ unwords
    [padLeft 5 (show (mat ! (i,j))) | j <- [0..cols-1]]) [0..rows-1]
  where padLeft w s = replicate (w - length s) ' ' ++ s

naiveMatmul :: Array (Int,Int) Int -> Array (Int,Int) Int -> Int -> Int -> Int -> Array (Int,Int) Int
naiveMatmul a b nn mm kk = array ((0,0),(nn-1,mm-1))
  [((i,j), sum [a!(i,kv) * b!(kv,j) | kv <- [0..kk-1]])
  | i <- [0..nn-1], j <- [0..mm-1]]
