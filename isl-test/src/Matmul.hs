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
import Isl.HighLevel.Pure (PConjunction(..), PDisjunction(..))
import Isl.Scan

-- | The matmul iteration domain as a pure Haskell constraint set.
--
-- Parameters (alphabetical): K=0, M=1, N=2
-- Dimensions: i=0, j=1, k=2
--
-- { [i, j, k] : 0 <= i <= N-1 and 0 <= j <= M-1 and 0 <= k <= K-1 }
matmulDomain :: PConjunction '["K", "M", "N"] 3
matmulDomain = PConjunction $ Conjunction
  [ -- i >= 0
    InequalityConstraint (Ix (SetDim 0))
  , -- i <= N - 1  ⟺  N - 1 - i >= 0
    InequalityConstraint (Add (Add (Ix (SetParam 2)) (Constant (-1))) (Mul (-1) (Ix (SetDim 0))))
  , -- j >= 0
    InequalityConstraint (Ix (SetDim 1))
  , -- j <= M - 1  ⟺  M - 1 - j >= 0
    InequalityConstraint (Add (Add (Ix (SetParam 1)) (Constant (-1))) (Mul (-1) (Ix (SetDim 1))))
  , -- k >= 0
    InequalityConstraint (Ix (SetDim 2))
  , -- k <= K - 1  ⟺  K - 1 - k >= 0
    InequalityConstraint (Add (Add (Ix (SetParam 0)) (Constant (-1))) (Mul (-1) (Ix (SetDim 2))))
  ]

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
  let params = [fromIntegral k, fromIntegral m, fromIntegral n]

  matC <- do
    c <- newArray ((0,0),(n-1,m-1)) 0 :: IO (IOUArray (Int,Int) Int)
    -- scanFold visits every (i, j, k) in the domain
    scanFold scanner params
      (\io point -> io >> case point of
          [i, j, kk] -> do
            let ii = fromIntegral i; jj = fromIntegral j; kkk = fromIntegral kk
            old <- readArray c (ii, jj)
            writeArray c (ii, jj) (old + matA ! (ii, kkk) * matB ! (kkk, jj))
          _ -> error "unexpected point shape"
      ) (return ())
    unsafeFreeze c :: IO (Array (Int,Int) Int)

  putStrLn $ "\nC = A × B (" ++ show n ++ "×" ++ show m ++ "):"
  printMatrix matC n m

  -- Verify
  let expected = naiveMatmul matA matB n m k
  putStrLn $ if elems matC == elems expected
    then "\nResult matches naive matmul."
    else "\nMISMATCH!"

  -- Show the scanner structure
  putStrLn "\n--- Scanner structure ---"
  let Scanner [nest] = scanner
  putStrLn $ "Params: " ++ show (lnParams nest) ++ ", Dims: " ++ show (lnDims nest)
  mapM_ (\lvl -> do
    putStrLn $ "  dim " ++ show (llDim lvl) ++ ":"
    putStrLn $ "    lowers: " ++ show (llLowerBounds lvl)
    putStrLn $ "    uppers: " ++ show (llUpperBounds lvl)
    ) (lnLevels nest)

printMatrix :: Show a => Array (Int,Int) a -> Int -> Int -> IO ()
printMatrix mat rows cols =
  mapM_ (\i -> do
    let row = [show (mat ! (i,j)) | j <- [0..cols-1]]
    putStrLn $ "  " ++ unwords (map (padLeft 5) row)
  ) [0..rows-1]
  where padLeft w s = replicate (w - length s) ' ' ++ s

naiveMatmul :: Array (Int,Int) Int -> Array (Int,Int) Int -> Int -> Int -> Int -> Array (Int,Int) Int
naiveMatmul a b nn mm kk = array ((0,0),(nn-1,mm-1))
  [((i,j), sum [a!(i,k) * b!(k,j) | k <- [0..kk-1]])
  | i <- [0..nn-1], j <- [0..mm-1]]
