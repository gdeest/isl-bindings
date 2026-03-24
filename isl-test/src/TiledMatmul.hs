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
import Isl.HighLevel.Pure (PConjunction(..), PDisjunction(..))
import Isl.Scan
import Isl.Scan.FSM (scanFSM, scanFoldFSM)

-- | Tiled matmul domain with compile-time tile size.
--
-- Parameters (alphabetical): K=0, M=1, N=2
-- Dimensions: ti=0, tj=1, i=2, j=3, k=4
--
-- Since T is a literal constant, T*ti is just Mul T (Ix ti) — perfectly affine.
tiledMatmulDomain :: Integer -> PConjunction '["K", "M", "N"] 5
tiledMatmulDomain tileSize = PConjunction $ Conjunction
  -- ti >= 0
  [ InequalityConstraint (Ix (SetDim 0))
  -- N - 1 - T*ti >= 0  (ti <= ⌊(N-1)/T⌋, but we express it as T*ti <= N-1)
  , InequalityConstraint (Add (Add (Ix (SetParam 2)) (Constant (-1)))
                              (Mul (-tileSize) (Ix (SetDim 0))))
  -- tj >= 0
  , InequalityConstraint (Ix (SetDim 1))
  -- M - 1 - T*tj >= 0
  , InequalityConstraint (Add (Add (Ix (SetParam 1)) (Constant (-1)))
                              (Mul (-tileSize) (Ix (SetDim 1))))
  -- i - T*ti >= 0  (i >= T*ti)
  , InequalityConstraint (Add (Ix (SetDim 2))
                              (Mul (-tileSize) (Ix (SetDim 0))))
  -- T*ti + T - 1 - i >= 0  (i <= T*ti + T - 1)
  , InequalityConstraint (Add (Add (Mul tileSize (Ix (SetDim 0))) (Constant (tileSize - 1)))
                              (Mul (-1) (Ix (SetDim 2))))
  -- N - 1 - i >= 0  (i <= N - 1, clamps the last tile)
  , InequalityConstraint (Add (Add (Ix (SetParam 2)) (Constant (-1)))
                              (Mul (-1) (Ix (SetDim 2))))
  -- j - T*tj >= 0
  , InequalityConstraint (Add (Ix (SetDim 3))
                              (Mul (-tileSize) (Ix (SetDim 1))))
  -- T*tj + T - 1 - j >= 0
  , InequalityConstraint (Add (Add (Mul tileSize (Ix (SetDim 1))) (Constant (tileSize - 1)))
                              (Mul (-1) (Ix (SetDim 3))))
  -- M - 1 - j >= 0
  , InequalityConstraint (Add (Add (Ix (SetParam 1)) (Constant (-1)))
                              (Mul (-1) (Ix (SetDim 3))))
  -- k >= 0
  , InequalityConstraint (Ix (SetDim 4))
  -- K - 1 - k >= 0
  , InequalityConstraint (Add (Add (Ix (SetParam 0)) (Constant (-1)))
                              (Mul (-1) (Ix (SetDim 4))))
  ]

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
  let params = [fromIntegral k, fromIntegral m, fromIntegral n]

  -- Trace the FSM iteration to show state transitions and face structure
  putStrLn "\nFSM iteration (showing carry transitions):"
  let Scanner [nest] = scanner
      allPoints = scanFSM nest params
      -- Show transitions: detect which dimension changed (= carry dimension)
      transitions = zipWith detectCarry allPoints (tail allPoints)
  mapM_ (\(pt, carry) -> case pt of
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
    scanFoldFSM nest params
      (\io point -> io >> case point of
          [_ti, _tj, i, j, kk] -> do
            let ii = fromIntegral i; jj = fromIntegral j; kkk = fromIntegral kk
            old <- readArray c (ii, jj)
            writeArray c (ii, jj) (old + matA ! (ii, kkk) * matB ! (kkk, jj))
          _ -> error "unexpected point shape"
      ) (return ())
    unsafeFreeze c :: IO (Array (Int,Int) Int)

  putStrLn $ "\nC = A × B (" ++ show n ++ "×" ++ show m ++ "):"
  printMatrix matC n m

  let expected = naiveMatmul matA matB n m k
  putStrLn $ if elems matC == elems expected
    then "\nResult matches naive matmul."
    else "\nMISMATCH!"

  -- Show the scanner structure
  putStrLn "\n--- Scanner loop nest ---"
  let Scanner [nest] = scanner
      dimNames = ["ti", "tj", "i", "j", "k"]
      paramNames = ["K", "M", "N"]
  mapM_ (\lvl -> do
    let dn = dimNames !! llDim lvl
    putStrLn $ "  for " ++ dn ++ " ="
    putStrLn $ "    lower: " ++ showBounds paramNames dimNames (llLowerBounds lvl)
    putStrLn $ "    upper: " ++ showBounds paramNames dimNames (llUpperBounds lvl)
    ) (lnLevels nest)

showBounds :: [String] -> [String] -> [AffineBound] -> String
showBounds pnames dnames bounds =
  case bounds of
    []  -> "(none)"
    [b] -> showBound pnames dnames b
    bs  -> "max/min(" ++ unwords (map (showBound pnames dnames) bs) ++ ")"

showBound :: [String] -> [String] -> AffineBound -> String
showBound pnames dnames (AffineBound lcs pcs c d) =
  let terms = [show coeff ++ "*" ++ (dnames !! i) | (coeff, i) <- lcs]
           ++ [show coeff ++ "*" ++ (pnames !! i) | (coeff, i) <- pcs]
           ++ [show c | c /= 0]
      expr = if null terms then "0" else unwords (map withSign terms)
      withSign s@('-':_) = s
      withSign s = "+" ++ s
  in if d == 1 then dropPlus expr
     else "⌊(" ++ dropPlus expr ++ ")/" ++ show d ++ "⌋"
  where dropPlus ('+':s) = s
        dropPlus s = s

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
