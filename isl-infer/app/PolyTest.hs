{-# LANGUAGE ForeignFunctionInterface #-}

-- | Correctness test: compare polyhedral-generated packed matvec
-- against the hand-crafted version on synthetic Q8 data.
-- Tests multiple dimension configurations including edge cases.
module Main where

import Data.Int (Int8)
import Data.Word (Word8, Word16)
import Foreign.Marshal.Alloc (mallocBytes, callocBytes, free)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import Isl.Infer.Arch (zen5, archTiles, Tiles(..))
import Isl.Infer.Kernel.GEMM (WeightQuant(..))
import Isl.Infer.Kernel.Packed (compilePackedMatvecWithQ, CompiledPackedMatvec(..), cpmFn)
import Isl.Infer.Kernel.PackedPoly (compilePackedMatvecPoly, CompiledPackedMatvecPoly(..))

-- | IEEE 754 half-precision: 1.0
f16One :: Word16
f16One = 0x3C00

-- | Build synthetic Q8_0 weights: scale=1.0, qs[v] = ((j*7 + kb*3 + v) mod 5) - 2
-- Varies by row and block to avoid trivial cancellation.
buildQ8Weights :: Int -> Int -> IO (Ptr Word8)
buildQ8Weights n kBlocks = do
  let blockSize = 34
  buf <- callocBytes (n * kBlocks * blockSize)
  sequence_ [ do
      let offset = (j * kBlocks + kb) * blockSize
      pokeByteOff buf offset f16One
      sequence_ [ do
          let val = fromIntegral (((j * 7 + kb * 3 + v) `mod` 5) - 2) :: Int8
          pokeByteOff buf (offset + 2 + v) val
        | v <- [0..31] ]
    | j <- [0..n-1], kb <- [0..kBlocks-1] ]
  return buf

-- | Build input vector: x[i] = ((i * 3 + 1) mod 11) * 0.1
buildInput :: Int -> IO (Ptr Float)
buildInput kBlocks = do
  let k = kBlocks * 32
  buf <- mallocBytes (k * 4)
  pokeArray buf [fromIntegral ((i * 3 + 1) `mod` 11) * 0.1 | i <- [0..k-1]]
  return buf

-- | Run one comparison test, returns True on pass.
runTest :: String -> Int -> Int -> IO Bool
runTest label n kBlocks = do
  let arch = zen5
      tiles = archTiles arch n (kBlocks * 32)
      tj = tileJ tiles
      tk = tileK tiles

  printf "  %-40s N=%-5d KB=%-4d TJ=%-3d TK=%-4d ... " label n kBlocks tj tk

  wBuf <- buildQ8Weights n kBlocks
  xBuf <- buildInput kBlocks

  oldK <- compilePackedMatvecWithQ WQ8 arch tiles n kBlocks
  newK <- compilePackedMatvecPoly WQ8 arch tiles n kBlocks

  outOld <- callocBytes (n * 4) :: IO (Ptr Float)
  outNew <- callocBytes (n * 4) :: IO (Ptr Float)

  cpmFn oldK outOld xBuf (castPtr wBuf) (fromIntegral n) (fromIntegral kBlocks)
  cpmpFn newK outNew xBuf (castPtr wBuf) (fromIntegral n) (fromIntegral kBlocks)

  oldVals <- peekArray n outOld
  newVals <- peekArray n outNew

  let diffs = zipWith (\a b -> abs (a - b)) oldVals newVals
      maxErr = maximum diffs
      nMismatch = length (filter (> 0) diffs)
      maxOld = maximum (map abs oldVals)
      relErr = if maxOld > 0 then maxErr / maxOld else maxErr

  free wBuf; free xBuf; free outOld; free outNew

  if maxErr == 0
    then do
      putStrLn "PASS (bitwise)"
      return True
    else if relErr < 1e-6
      then do
        printf "PASS (rel=%.1e)\n" relErr
        return True
      else do
        printf "FAIL (max=%.3e, rel=%.3e, mismatches=%d/%d)\n" maxErr relErr nMismatch n
        printf "    First 5 old: %s\n" (show (take 5 oldVals))
        printf "    First 5 new: %s\n" (show (take 5 newVals))
        return False

-- | Compute reference matvec in Haskell for small dimensions.
-- This validates both the hand-crafted AND polyhedral kernels against ground truth.
referenceMatvec :: Int -> Int -> [Int8] -> [Float] -> [Float]
referenceMatvec n kBlocks qsFlat xVec =
  [ sum [ fromIntegral (qsFlat !! (j * kBlocks * 32 + i)) * (xVec !! i)
        | i <- [0 .. kBlocks * 32 - 1] ]
  | j <- [0 .. n - 1] ]

-- | Cross-check both kernels against a Haskell reference for small dimensions.
runReferenceTest :: IO Bool
runReferenceTest = do
  let n = 16; kBlocks = 2  -- small enough for Haskell reference
      arch = zen5
      tiles = archTiles arch n (kBlocks * 32)

  printf "  %-40s N=%-5d KB=%-4d ... " "Reference cross-check" n kBlocks

  wBuf <- buildQ8Weights n kBlocks
  xBuf <- buildInput kBlocks

  -- Extract the raw quantized values for Haskell reference
  -- Q8 layout: [scale(2), qs(32)] per block, row-major [N, KB]
  let blockSize = 34
  rawW <- peekArray (n * kBlocks * blockSize) (castPtr wBuf :: Ptr Word8)
  -- Extract just the qs values (skip 2-byte scale per block, scale=1.0 so no dequant needed)
  let toSigned byte = if byte >= 128 then fromIntegral byte - 256 else fromIntegral byte :: Int8
      qsFlat = [ toSigned (rawW !! ((j * kBlocks + kb) * blockSize + 2 + v))
                | j <- [0..n-1], kb <- [0..kBlocks-1], v <- [0..31] ]

  xVals <- peekArray (kBlocks * 32) xBuf
  let hsRef = referenceMatvec n kBlocks qsFlat xVals

  -- Run both kernels
  oldK <- compilePackedMatvecWithQ WQ8 arch tiles n kBlocks
  newK <- compilePackedMatvecPoly WQ8 arch tiles n kBlocks

  outOld <- callocBytes (n * 4) :: IO (Ptr Float)
  outNew <- callocBytes (n * 4) :: IO (Ptr Float)

  cpmFn oldK outOld xBuf (castPtr wBuf) (fromIntegral n) (fromIntegral kBlocks)
  cpmpFn newK outNew xBuf (castPtr wBuf) (fromIntegral n) (fromIntegral kBlocks)

  oldVals <- peekArray n outOld
  newVals <- peekArray n outNew

  free wBuf; free xBuf; free outOld; free outNew

  -- Compare both against Haskell reference
  let oldDiffs = zipWith (\a b -> abs (a - b)) oldVals hsRef
      newDiffs = zipWith (\a b -> abs (a - b)) newVals hsRef
      maxOldErr = maximum oldDiffs
      maxNewErr = maximum newDiffs
      maxRef = maximum (map abs hsRef)
      relOld = if maxRef > 0 then maxOldErr / maxRef else maxOldErr
      relNew = if maxRef > 0 then maxNewErr / maxRef else maxNewErr

  if relOld < 1e-5 && relNew < 1e-5
    then do
      printf "PASS (old_rel=%.1e, new_rel=%.1e)\n" relOld relNew
      return True
    else do
      printf "FAIL (old_rel=%.1e, new_rel=%.1e)\n" relOld relNew
      printf "    HS ref first 5:  %s\n" (show (take 5 hsRef))
      printf "    Old first 5:     %s\n" (show (take 5 oldVals))
      printf "    New first 5:     %s\n" (show (take 5 newVals))
      return False

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "=== Polyhedral Packed Matvec Correctness Tests ==="
  putStrLn ""

  -- Haskell reference cross-check (validates both kernels against ground truth)
  putStrLn "--- Reference tests ---"
  r0 <- runReferenceTest
  putStrLn ""

  -- Comparison tests: polyhedral vs hand-crafted
  putStrLn "--- Dimension sweep (old vs poly) ---"
  results <- sequence
    [ -- Aligned cases (N divisible by TJ)
      runTest "Aligned, small"       48  4
    , runTest "Aligned, medium"      192 16
    , runTest "Aligned, large"       4096 128
      -- Unaligned cases (remainder tiles)
    , runTest "Remainder, N=1"       1   4
    , runTest "Remainder, N=7"       7   4
    , runTest "Remainder, N=49"      49  4
    , runTest "Remainder, N=200"     200 20
    , runTest "Remainder, N=4097"    4097 128
      -- Edge: KB not divisible by TKB
    , runTest "KB remainder, N=100"  100 7
    , runTest "KB remainder, N=48"   48  3
      -- Stress: single tile
    , runTest "Single j-tile"        32  4
    , runTest "Single k-tile"        100 2
      -- Stress: large
    , runTest "Large, N=8192"        8192 128
    ]

  putStrLn ""
  let nPass = length (filter id (r0 : results))
      nTotal = length (r0 : results)
  printf "=== %d / %d tests passed ===\n" nPass nTotal

  if nPass == nTotal
    then exitSuccess
    else exitFailure
