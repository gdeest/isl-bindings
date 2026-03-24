{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tiled matrix multiplication via polyhedral scanning.
--
-- Tiling is just a higher-dimensional polyhedron:
--
--   for ti = 0 to ⌊(N-1)/T⌋:
--     for tj = 0 to ⌊(M-1)/T⌋:
--       for i = T*ti to min(T*ti+T-1, N-1):
--         for j = T*tj to min(T*tj+T-1, M-1):
--           for k = 0 to K-1:
--             C[i][j] += A[i][k] * B[k][j]
--
-- The "min" is handled naturally — two upper bounds, scanner takes the minimum.
module Main where

import Data.Array (Array, array, (!), elems, listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.IO (IOUArray)
import Data.Array.Unsafe (unsafeFreeze)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction, PDisjunction(..), mkPConjunction)
import Isl.Scan

-- | Tiled matmul domain with compile-time tile size.
--
-- Parameters (alphabetical): K=0, M=1, N=2
-- Dimensions: ti=0, tj=1, i=2, j=3, k=4
tiledMatmulDomain :: Integer -> PConjunction '["K", "M", "N"] 5
tiledMatmulDomain t = mkPConjunction @'["K","M","N"] @5 $
  \(kp :- mp :- np :- Nil) (ti :- tj :- i :- j :- k :- Nil) ->
    -- Tile index bounds
        idx ti >=: cst 0 &&: t *: idx ti <=: idx np -: cst 1
    &&: idx tj >=: cst 0 &&: t *: idx tj <=: idx mp -: cst 1
    -- i within tile and within N
    &&: idx i >=: t *: idx ti &&: idx i <=: t *: idx ti +: cst (t - 1)
    &&: idx i <=: idx np -: cst 1
    -- j within tile and within M
    &&: idx j >=: t *: idx tj &&: idx j <=: t *: idx tj +: cst (t - 1)
    &&: idx j <=: idx mp -: cst 1
    -- k range
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1

main :: IO ()
main = do
  let t = 2  -- tile size
      n = 5; m = 6; k = 3

  -- Build scanner
  let scanner = mkScanner (PDisjunction [tiledMatmulDomain t])

  -- Matrices: A is N×K, B is K×M
  let matA = listArray ((0,0),(n-1,k-1))
        [ 1, 2, 3
        , 4, 5, 6
        , 7, 8, 9
        , 10, 11, 12
        , 13, 14, 15 ] :: Array (Int,Int) Int
      matB = listArray ((0,0),(k-1,m-1))
        [ 1, 2, 3, 4, 5, 6
        , 7, 8, 9, 10, 11, 12
        , 13, 14, 15, 16, 17, 18 ] :: Array (Int,Int) Int

  putStrLn $ "Tiled matmul with T=" ++ show t
  putStrLn $ "A (" ++ show n ++ "×" ++ show k ++ "):"
  printMatrix matA n k
  putStrLn $ "\nB (" ++ show k ++ "×" ++ show m ++ "):"
  printMatrix matB k m

  -- Parameters: K=0, M=1, N=2
  let params = mkVec @3 [fromIntegral k, fromIntegral m, fromIntegral n]

  -- Trace the FSM iteration to show state transitions and face structure
  putStrLn "\nFSM iteration (showing carry transitions):"
  let Scanner [nest] = scanner
      allPoints = scanFSM nest params
      -- Show transitions: detect which dimension changed (= carry dimension)
      transitions = zipWith (\p1 p2 -> detectCarry (toList p1) (toList p2)) allPoints (tail allPoints)
  mapM_ (\(pt, carry) -> case toList pt of
    [ti,tj,i,j,kk] ->
      let prefix = "  [ti=" ++ show ti ++ " tj=" ++ show tj
                 ++ " i=" ++ show i ++ " j=" ++ show j ++ " k=" ++ show kk ++ "]"
          dimNames = ["ti", "tj", "i", "j", "k"]
          suffix = case carry of
            Just d  -> " → carry " ++ (dimNames !! d)
            Nothing -> ""
      in putStrLn $ prefix ++ suffix
    _ -> return ()
    ) (take 30 (zip allPoints (map Just transitions ++ [Nothing])))
  putStrLn $ "  ... (" ++ show (length allPoints) ++ " total iterations)"

  -- Do the actual matmul via FSM scanner
  matC <- do
    c <- newArray ((0,0),(n-1,m-1)) 0 :: IO (IOUArray (Int,Int) Int)
    scanForM_FSM nest params $ \(Vec [_ti, _tj, i, j, kk]) -> do
      let ii = fromIntegral i; jj = fromIntegral j; kkk = fromIntegral kk
      old <- readArray c (ii, jj)
      writeArray c (ii, jj) (old + matA ! (ii, kkk) * matB ! (kkk, jj))
    unsafeFreeze c :: IO (Array (Int,Int) Int)

  putStrLn $ "\nC = A × B (" ++ show n ++ "×" ++ show m ++ "):"
  printMatrix matC n m

  let expected = naiveMatmul matA matB n m k
  putStrLn $ if elems matC == elems expected
    then "\nResult matches naive matmul."
    else "\nMISMATCH!"

  -- Show the scanner structure using pretty-printing
  putStrLn "\n--- Scanner loop nest ---"
  putStrLn $ prettyScanner ["K", "M", "N"] ["ti", "tj", "i", "j", "k"] scanner

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

-- | Detect which dimension carried between two consecutive points.
-- The carry dimension is the outermost dimension that changed.
detectCarry :: [Integer] -> [Integer] -> Int
detectCarry prev next = go 0
  where
    go i | i >= length prev = length prev - 1
         | prev !! i /= next !! i = i
         | otherwise = go (i + 1)
