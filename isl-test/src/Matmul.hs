{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Matrix multiplication via polyhedral scanning.
--
-- C[i][j] += A[i][k] * B[k][j]
-- Domain: { [i, j, k] : 0 <= i < N and 0 <= j < M and 0 <= k < K }
module Main where

import Data.Array (Array, array, (!), elems, listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe (unsafeFreeze)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction, PDisjunction(..), mkPConjunction)
import Isl.Scan

-- | The matmul iteration domain.
--
-- Parameters (alphabetical): K=0, M=1, N=2
-- Dimensions: i=0, j=1, k=2
matmulDomain :: PConjunction '["K", "M", "N"] 3
matmulDomain = mkPConjunction @'["K","M","N"] @3 $
  \(kp :- mp :- np :- Nil) (i :- j :- k :- Nil) ->
    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1

main :: IO ()
main = do
  -- Build scanner from the domain
  let scanner = mkScanner (PDisjunction [matmulDomain])

  -- Define some small matrices
  let n = 3; m = 4; k = 2
      -- A is N×K
      matA = listArray ((0,0),(n-1,k-1))
        [ 1, 2
        , 3, 4
        , 5, 6 ] :: Array (Int,Int) Int
      -- B is K×M
      matB = listArray ((0,0),(k-1,m-1))
        [ 7,  8,  9, 10
        , 11, 12, 13, 14 ] :: Array (Int,Int) Int

  putStrLn $ "A (" ++ show n ++ "×" ++ show k ++ "):"
  printMatrix matA n k
  putStrLn $ "\nB (" ++ show k ++ "×" ++ show m ++ "):"
  printMatrix matB k m

  -- Use the scanner to drive matmul
  -- Parameters in alphabetical order: K, M, N
  let params = mkVec @3 [fromIntegral k, fromIntegral m, fromIntegral n]

  matC <- do
    c <- newArray ((0,0),(n-1,m-1)) 0 :: IO (IOUArray (Int,Int) Int)
    scanForM_ scanner params $ \(Vec [ii, jj, kk]) -> do
      let i = fromIntegral ii; j = fromIntegral jj; kv = fromIntegral kk
      old <- readArray c (i, j)
      writeArray c (i, j) (old + matA ! (i, kv) * matB ! (kv, j))
    unsafeFreeze c :: IO (Array (Int,Int) Int)

  putStrLn $ "\nC = A × B (" ++ show n ++ "×" ++ show m ++ "):"
  printMatrix matC n m

  -- Verify
  let expected = naiveMatmul matA matB n m k
  putStrLn $ if elems matC == elems expected
    then "\nResult matches naive matmul."
    else "\nMISMATCH!"

  -- Show the scanner structure using pretty-printing
  putStrLn "\n--- Scanner loop nest ---"
  putStrLn $ prettyScanner ["K", "M", "N"] ["i", "j", "k"] scanner

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
