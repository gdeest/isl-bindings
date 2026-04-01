{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

-- | Full-parameter auto-tuner for isl-infer.
--
-- Performs coordinate descent over the complete parameter space:
-- strategy, schedule tiles, VNNI, KV cache mode, FFN quantization,
-- GEMM batch position, and attention tile size.
--
-- Each configuration is benchmarked by running @benchTokens@ decode
-- steps and measuring wall-clock tok/s. Coordinate descent sweeps
-- one parameter at a time (fixing all others at current best),
-- repeating until convergence.
--
-- Typical run: ~50-100 configurations × ~2s each = 2-3 minutes.
module Isl.Infer.Tune
  ( TuneConfig(..)
  , defaultTuneConfig
  , TuneResult(..)
  , tune
  , benchmarkAll
  , configLabel
  , toSchedule
  ) where

import Data.Int (Int64)
import Data.List (maximumBy, intercalate)
import Data.Ord (comparing)
import Data.Word (Word8, Word64)
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr)
import Text.Printf (printf)

import Isl.Infer.Arch
import Isl.Infer.Compile
import Isl.Infer.Forward
import Isl.Infer.GGUF (GGUFFile)
import Isl.Infer.Model (LlamaConfig(..))
import Isl.Infer.Schedule
import Isl.Infer.Kernel.GEMM (WeightQuant(..))

-- ---------------------------------------------------------------------------
-- Timing
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "clock_gettime"
  c_clock_gettime_tune :: Int -> Ptr () -> IO Int

getTimeNs :: IO Word64
getTimeNs = do
  buf <- mallocBytes 16
  _ <- c_clock_gettime_tune 1 (castPtr buf)
  [sec, nsec] <- peekArray 2 (castPtr buf :: Ptr CLong)
  free buf
  return (fromIntegral sec * 1000000000 + fromIntegral nsec)

-- ---------------------------------------------------------------------------
-- Tunable parameter space
-- ---------------------------------------------------------------------------

-- | Tuning configuration — pure performance knobs only.
--
-- Quantization (FFN Q4, KV Q8/Q4) and speculation are *quality* choices
-- that affect model output, so they are NOT swept here. They are passed
-- as fixed constraints from the user's current settings.
data TuneConfig = TuneConfig
  { -- Tunable (swept by coordinate descent)
    tcStrategy  :: !MatvecStrategy
  , tcTileJ     :: !Int
  , tcTileK     :: !Int
  , tcParallel  :: !Bool
  , tcVnni      :: !Bool
    -- Fixed (inherited from user's current settings, not swept)
  , tcFFNQuant  :: !WeightQuant
  , tcKVMode    :: !KVCacheMode
  } deriving (Show)

-- | Conservative starting point: packed strategy, default tiles, no VNNI.
-- FFN quant and KV mode are set by the caller (user's current choices).
defaultTuneConfig :: WeightQuant -> KVCacheMode -> TuneConfig
defaultTuneConfig ffnQ kvM = TuneConfig
  { tcStrategy  = StrategyOriginal
  , tcTileJ     = 128
  , tcTileK     = 0
  , tcParallel  = True
  , tcVnni      = False
  , tcFFNQuant  = ffnQ
  , tcKVMode    = kvM
  }

-- | Result of a full tuning run.
data TuneResult = TuneResult
  { trConfig    :: !TuneConfig
  , trTokPerSec :: !Double
  , trSweeps    :: !Int
  } deriving (Show)

-- | Convert TuneConfig to a MatvecSchedule.
toSchedule :: TuneConfig -> MatvecSchedule
toSchedule tc = MatvecSchedule
  { schName     = "tuned"
  , schTileJ    = tcTileJ tc
  , schTileJ2   = 0
  , schTileK    = tcTileK tc
  , schParallel = tcParallel tc
  , schSimd     = True
  , schVnni     = tcVnni tc
  }

-- | Human-readable label for a TuneConfig.
configLabel :: TuneConfig -> String
configLabel tc = intercalate ", "
  [ stratLabel, "tJ=" ++ show (tcTileJ tc), "tK=" ++ show (tcTileK tc)
  , if tcParallel tc then "OMP" else "serial"
  , if tcVnni tc then "VNNI" else "float"
  , ffnLabel, kvLabel
  ]
  where
    stratLabel = case tcStrategy tc of
      StrategyOriginal  -> "original"
      StrategyPacked    -> "packed"
      StrategyPackedPoly -> "packed-poly"
      StrategyAutotuned -> "autotuned"
    ffnLabel = case tcFFNQuant tc of
      WQ8  -> "FFN:Q8"
      WQ4  -> "FFN:Q4"
      WQ4K -> "FFN:Q4K"
    kvLabel = case (kvKQuant (tcKVMode tc), kvVQuant (tcKVMode tc)) of
      (QFloat32, QFloat32) -> "KV:f32"
      (QQ8, QQ8)           -> "KV:q8"
      (QQ4, QQ4)           -> "KV:q4"
      (QQ8, QQ4)           -> "KV:k8v4"
      (QQ4, QQ8)           -> "KV:k4v8"
      _                    -> "KV:custom"

-- ---------------------------------------------------------------------------
-- Benchmark a single configuration
-- ---------------------------------------------------------------------------

-- | Benchmark a configuration by compiling kernels, running @nTokens@ decode
-- steps, and returning tok/s. Reuses the provided InferState's GGUF data
-- and tokenizer state, but recompiles all kernels.
benchTuneConfig :: Arch -> LlamaConfig -> GGUFFile -> InferState
                -> TuneConfig -> Int -> IO Double
benchTuneConfig arch cfg gf is tc nTokens = do
  let sch = toSchedule tc
  -- Compile kernels for this configuration
  kernels <- compileKernelsWith (tcStrategy tc) arch (tcKVMode tc) (tcFFNQuant tc) cfg sch
  -- Create a fresh InferState with these kernels
  is' <- initInferState (tcKVMode tc) cfg gf kernels

  -- Run a short decode sequence: feed token 1 (BOS), then decode nTokens
  forward is' 1 0  -- BOS token at position 0

  t0 <- getTimeNs
  let go :: Int -> Int -> IO Int
      go pos 0 = return pos
      go pos remaining = do
        -- Use a fixed token (doesn't matter for timing — same compute per token)
        forward is' 42 pos
        go (pos + 1) (remaining - 1)
  _ <- go 1 nTokens
  t1 <- getTimeNs

  let elapsed_ms = fromIntegral (t1 - t0) / 1e6
      toks = fromIntegral nTokens * 1000.0 / elapsed_ms

  return toks

-- ---------------------------------------------------------------------------
-- Coordinate descent tuner
-- ---------------------------------------------------------------------------

-- | Run coordinate descent over the full parameter space.
--
-- Sweeps each parameter dimension independently, always keeping the best
-- value found so far. Repeats until a full sweep yields no improvement.
--
-- Parameters are swept in order of estimated impact:
-- 1. Strategy (original vs packed vs autotuned)
-- 2. VNNI (int8×int8 accumulation)
-- 3. FFN quantization (Q8 vs Q4 — halves bandwidth)
-- 4. KV cache mode (float32 vs Q8 vs Q4)
-- 5. Tile J (parallelism granularity)
-- 6. Tile K (cache blocking depth)
-- 7. Parallel (OpenMP on/off — for small matrices)
-- | Run coordinate descent over the performance parameter space.
--
-- Quantization and KV mode are fixed from the user's current settings
-- (they affect model quality, not pure performance tuning).
-- Speculation is also excluded (it's a generation strategy, not a kernel knob).
tune :: Arch -> LlamaConfig -> GGUFFile -> InferState
     -> WeightQuant -> KVCacheMode -> Int -> IO TuneResult
tune arch cfg gf is ffnQ kvM benchTokens = do
  putStrLn "=== Full parameter auto-tune ==="
  printf "  Benchmark: %d decode tokens per config\n" benchTokens
  printf "  Fixed: FFN=%s, KV=%s (not swept — quality choices)\n"
    (show ffnQ) (show kvM)

  -- Initial benchmark
  let tc0 = defaultTuneConfig ffnQ kvM
  printf "  Initial config: %s\n" (configLabel tc0)
  tps0 <- benchTuneConfig arch cfg gf is tc0 benchTokens
  printf "  Initial: %.2f tok/s\n\n" tps0

  -- Coordinate descent
  (bestTc, bestTps, sweeps) <- coordinateDescent tc0 tps0 1

  putStrLn ""
  printf "=== Tuning complete (%d sweeps) ===\n" sweeps
  printf "  Best: %.2f tok/s\n" bestTps
  printf "  Config: %s\n" (configLabel bestTc)

  return TuneResult
    { trConfig    = bestTc
    , trTokPerSec = bestTps
    , trSweeps    = sweeps
    }
  where
    bench = benchTuneConfig arch cfg gf is

    coordinateDescent :: TuneConfig -> Double -> Int -> IO (TuneConfig, Double, Int)
    coordinateDescent tc tps sweepNum = do
      printf "--- Sweep %d (current best: %.2f tok/s) ---\n" sweepNum tps

      -- Sweep each parameter dimension (high-impact first)
      (tc1, tps1) <- sweepStrategy   tc  tps
      (tc2, tps2) <- sweepVnni       tc1 tps1
      (tc3, tps3) <- sweepTileJ      tc2 tps2
      (tc4, tps4) <- sweepTileK      tc3 tps3
      (tc5, tps5) <- sweepParallel   tc4 tps4

      if tps5 > tps * 1.01  -- >1% improvement → keep sweeping
        then coordinateDescent tc5 tps5 (sweepNum + 1)
        else return (tc5, tps5, sweepNum)

    -- Sweep a single dimension: try all values, keep the best.
    sweepDim :: String -> TuneConfig -> Double -> [TuneConfig] -> IO (TuneConfig, Double)
    sweepDim name currentBest currentTps candidates = do
      results <- mapM (\tc -> do
        tps <- bench tc benchTokens
        printf "    %s → %s: %.2f tok/s%s\n"
          name (configLabel tc) tps
          (if tps > currentTps then " ★" else "" :: String)
        return (tc, tps)
        ) candidates
      let allResults = (currentBest, currentTps) : results
          (bestTc, bestTps) = maximumBy (comparing snd) allResults
      return (bestTc, bestTps)

    -- Parameter dimension sweeps
    sweepStrategy tc tps = sweepDim "strategy" tc tps
      [ tc { tcStrategy = s } | s <- [StrategyOriginal, StrategyPacked, StrategyPackedPoly, StrategyAutotuned]
      , s /= tcStrategy tc ]

    sweepVnni tc tps = sweepDim "vnni" tc tps
      [ tc { tcVnni = v } | v <- [True, False], v /= tcVnni tc ]

    sweepTileJ tc tps = sweepDim "tileJ" tc tps
      [ tc { tcTileJ = j } | j <- [0, 64, 128, 192, 256], j /= tcTileJ tc ]

    sweepTileK tc tps = sweepDim "tileK" tc tps
      [ tc { tcTileK = k } | k <- [0, 4, 8], k /= tcTileK tc ]

    sweepParallel tc tps = sweepDim "parallel" tc tps
      [ tc { tcParallel = p } | p <- [True, False], p /= tcParallel tc ]

-- ---------------------------------------------------------------------------
-- Full benchmark matrix
-- ---------------------------------------------------------------------------

-- | Run every parameter combination independently against a baseline.
-- Returns a list of (label, tok/s) pairs for analysis.
-- Each dimension is swept with all others at the baseline value.
benchmarkAll :: Arch -> LlamaConfig -> GGUFFile -> InferState
             -> WeightQuant -> KVCacheMode -> Int -> IO [(String, Double)]
benchmarkAll arch cfg gf is ffnQ kvM nTok = do
  let base = defaultTuneConfig ffnQ kvM
      bench tc = benchTuneConfig arch cfg gf is tc nTok

  putStrLn "=== Benchmark matrix ==="
  printf "  %d decode tokens per config, baseline: %s\n\n" nTok (configLabel base)

  -- Baseline
  baseTps <- bench base
  printf "  %-45s  %7.2f tok/s  (baseline)\n" (configLabel base) baseTps

  -- Sweep each dimension independently from baseline
  let dims :: [(String, [TuneConfig])]
      dims =
        [ ("strategy"
          , [ base { tcStrategy = s }
            | s <- [StrategyOriginal, StrategyPacked, StrategyPackedPoly, StrategyAutotuned] ])
        , ("vnni"
          , [ base { tcVnni = v } | v <- [False, True] ])
        , ("tileJ"
          , [ base { tcTileJ = j } | j <- [0, 64, 128, 192, 256] ])
        , ("tileK"
          , [ base { tcTileK = k } | k <- [0, 4, 8] ])
        , ("parallel"
          , [ base { tcParallel = p } | p <- [True, False] ])
        ]

  results <- concat <$> mapM (\(dimName, configs) -> do
    printf "\n  --- %s ---\n" dimName
    mapM (\tc -> do
      tps <- bench tc
      let delta = (tps - baseTps) / baseTps * 100.0
          marker | tps > baseTps * 1.02 = " ★"
                 | tps < baseTps * 0.98 = " ✗"
                 | otherwise            = "" :: String
      printf "  %-45s  %7.2f tok/s  (%+.1f%%)%s\n" (configLabel tc) tps delta marker
      return (configLabel tc, tps)
      ) configs
    ) dims

  -- Summary
  putStrLn "\n=== Summary ==="
  let allResults = (configLabel base, baseTps) : results
      (bestLabel, bestTps) = maximumBy (comparing snd) allResults
  printf "  Best:     %-45s  %.2f tok/s\n" bestLabel bestTps
  printf "  Baseline: %-45s  %.2f tok/s\n" (configLabel base) baseTps
  printf "  Speedup:  %.2fx\n" (bestTps / baseTps)

  return allResults
