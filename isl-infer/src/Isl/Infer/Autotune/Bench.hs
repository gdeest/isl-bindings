{-# LANGUAGE ForeignFunctionInterface #-}

-- | Candidate tile benchmarking for the GRASP autotuner.
--
-- Compiles a kernel with given tile parameters, benchmarks it
-- (median of 5 wall-clock measurements), then unloads the kernel.
-- Uses @clock_gettime(CLOCK_MONOTONIC)@ for accurate wall-clock
-- timing of OpenMP kernels.
module Isl.Infer.Autotune.Bench
  ( BenchResult(..)
  , benchCandidate
  ) where

import Data.Int (Int64)
import Data.List (sort)
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.Marshal.Alloc (mallocBytes, free, callocBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr, castPtr)
import Foreign.Storable (poke, peekByteOff, pokeByteOff)

import Isl.Infer.Arch
import Isl.Infer.Kernel.Packed (genPackedMatvec)
import Isl.Infer.Runtime

-- ---------------------------------------------------------------------------
-- Wall-clock timing via clock_gettime(CLOCK_MONOTONIC)
-- ---------------------------------------------------------------------------

-- struct timespec { time_t tv_sec; long tv_nsec; }
-- We allocate 16 bytes and read sec + nsec manually.

foreign import ccall unsafe "clock_gettime"
  c_clock_gettime :: CInt -> Ptr () -> IO CInt

clockMonotonic :: CInt
clockMonotonic = 1

-- | Get wall-clock time in nanoseconds.
getTimeNs :: IO Double
getTimeNs = do
  buf <- mallocBytes 16
  _ <- c_clock_gettime clockMonotonic buf
  sec  <- peekByteOff buf 0 :: IO CLong
  nsec <- peekByteOff buf 8 :: IO CLong
  free buf
  return $! fromIntegral sec * 1e9 + fromIntegral nsec

-- ---------------------------------------------------------------------------
-- Benchmark result
-- ---------------------------------------------------------------------------

-- | Result of benchmarking a tile configuration.
data BenchResult = BenchResult
  { brTiles    :: !Tiles
  , brMedianNs :: !Double
    -- ^ Median wall-clock time in nanoseconds (of 5 runs).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- FFI for calling the compiled kernel
-- ---------------------------------------------------------------------------

type MatvecFnC = Ptr Float -> Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> IO ()

foreign import ccall "dynamic"
  mkMatvecFn :: FunPtr MatvecFnC -> MatvecFnC

-- ---------------------------------------------------------------------------
-- Candidate benchmarking
-- ---------------------------------------------------------------------------

-- | Compile a kernel with the given tiles, benchmark it, and clean up.
--
-- Allocates scratch buffers with deterministic fill (no need for random —
-- we only care about relative timing, not correctness here).
benchCandidate :: Arch -> Tiles -> Int -> Int -> IO BenchResult
benchCandidate arch tiles n kBlocks = do
  let fname = "q8mv_packed_" ++ archName arch
      src   = genPackedMatvec arch tiles n kBlocks
  ck <- compileAndLoad fname src
  let fn = mkMatvecFn (castFunPtr (ckFuncPtr ck))

  -- Allocate scratch buffers
  let outBytes = n * 4            -- float[n]
      xBytes   = kBlocks * 32 * 4 -- float[k]
      wBytes   = n * kBlocks * 34 -- block_q8_0[n * kBlocks], 34 bytes each
  outBuf <- callocBytes outBytes
  xBuf   <- mallocBytes xBytes
  wBuf   <- callocBytes wBytes

  -- Deterministic fill for x (all 1.0f — simple, avoids NaN issues)
  let k = kBlocks * 32
  pokeArray (castPtr xBuf :: Ptr Float) (replicate k 1.0)

  -- Warm-up call
  fn outBuf xBuf wBuf (fromIntegral n) (fromIntegral kBlocks)

  -- 5 timed runs
  times <- sequence
    [ do t0 <- getTimeNs
         fn outBuf xBuf wBuf (fromIntegral n) (fromIntegral kBlocks)
         t1 <- getTimeNs
         return $! t1 - t0
    | _ <- [1..5 :: Int]
    ]

  -- Cleanup
  free outBuf
  free xBuf
  free wBuf
  unloadKernel ck

  -- Median of sorted times
  let sorted = sort times
      median = sorted !! 2  -- middle of 5

  return BenchResult
    { brTiles    = tiles
    , brMedianNs = median
    }
