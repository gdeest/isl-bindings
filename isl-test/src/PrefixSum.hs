{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Prefix sum (scanl (+) 0) via polyhedral scanning.
--
-- out[i] = sum(inp[0..i-1])  for i = 0..N
-- Domain: { [i, j] : 1 <= i <= N and 0 <= j < i }
module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction, PDisjunction(..), mkPConjunction)
import Isl.Scan

-- | The prefix sum domain: { [i, j] : 1 <= i <= N, 0 <= j <= i-1 }
--
-- Parameters: N=0
-- Dimensions: i=0, j=1
prefixSumDomain :: PConjunction '["N"] 2
prefixSumDomain = mkPConjunction @'["N"] @2 $
  \(np :- Nil) (i :- j :- Nil) ->
    idx i >=: cst 1 &&: idx i <=: idx np
    &&: idx j >=: cst 0 &&: idx j <=: idx i -: cst 1

main :: IO ()
main = do
  let scanner = mkScanner (PDisjunction [prefixSumDomain])

  let n = 8
      inp = V.fromList [1, 2, 3, 4, 5, 6, 7, 8 :: Int]

  putStrLn $ "Input:              " ++ show (V.toList inp)

  let params = mkVec @1 [fromIntegral n]

  out <- do
    v <- MV.replicate (n + 1) (0 :: Int)
    scanForM_ scanner params $ \(Vec [ii, jj]) -> do
      let i = fromIntegral ii; j = fromIntegral jj
      old <- MV.read v i
      MV.write v i (old + inp V.! j)
    V.freeze v

  putStrLn $ "scanl (+) 0:        " ++ show (V.toList out)

  -- Verify against Haskell's scanl
  let expected = V.fromList (scanl (+) 0 (V.toList inp))
  putStrLn $ if out == expected
    then "\nResult matches scanl (+) 0."
    else "\nMISMATCH! Expected: " ++ show (V.toList expected)

  putStrLn "\n--- Scanner loop nest ---"
  putStrLn $ prettyScanner ["N"] ["i", "j"] scanner
