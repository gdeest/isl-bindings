-- | GRASP autotuner for panel-packed microkernel tile parameters.
--
-- Replaces the heuristic 'archTiles' with empirical search when
-- 'StrategyAutotuned' is selected. The search space is small (~50
-- candidates) and each evaluation is fast (~100ms compile + timing),
-- so the full GRASP search completes in a few seconds per unique
-- matrix shape.
--
-- Results are cached by @(archName, n, k)@ so repeated calls for
-- the same shape (e.g., Q and O projections sharing @dim → dim@)
-- are free.
module Isl.Infer.Autotune
  ( autotune
  , autotuneWithConfig
  , AutotuneConfig(..)
  , defaultAutotuneConfig
  ) where

import Data.IORef
import Data.List (minimumBy, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import Isl.Infer.Arch
import Isl.Infer.Autotune.Bench
import Isl.Infer.Autotune.Search

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | GRASP autotuner configuration.
data AutotuneConfig = AutotuneConfig
  { atIterations :: !Int
    -- ^ Number of GRASP iterations (default: 5).
  , atAlpha      :: !Double
    -- ^ RCL threshold ratio (default: 0.8). Candidates with
    -- heuristic score >= alpha × bestScore enter the restricted
    -- candidate list.
  , atVerbose    :: !Bool
    -- ^ Print search progress (default: True).
  } deriving (Show)

-- | Default configuration: 5 iterations, alpha = 0.8, verbose.
defaultAutotuneConfig :: AutotuneConfig
defaultAutotuneConfig = AutotuneConfig
  { atIterations = 5
  , atAlpha      = 0.8
  , atVerbose    = True
  }

-- ---------------------------------------------------------------------------
-- Cache (global, keyed by archName × n × k)
-- ---------------------------------------------------------------------------

type CacheKey = (String, Int, Int)

{-# NOINLINE autotuneCache #-}
autotuneCache :: IORef (Map CacheKey Tiles)
autotuneCache = unsafePerformIO (newIORef Map.empty)

lookupCache :: CacheKey -> IO (Maybe Tiles)
lookupCache key = Map.lookup key <$> readIORef autotuneCache

insertCache :: CacheKey -> Tiles -> IO ()
insertCache key tiles =
  atomicModifyIORef' autotuneCache (\m -> (Map.insert key tiles m, ()))

-- ---------------------------------------------------------------------------
-- Simple LCG for RCL selection (avoids random dependency)
-- ---------------------------------------------------------------------------

-- | Linear congruential generator state.
newtype LCG = LCG { lcgState :: Int }

-- | Seed from current time (nanoseconds).
lcgFromTime :: IO LCG
lcgFromTime = do
  -- Use the autotuneCache IORef address as a poor-man's entropy source,
  -- combined with a simple counter.
  s <- atomicModifyIORef' lcgSeed (\n -> (n + 6364136223846793005, n))
  return (LCG s)

{-# NOINLINE lcgSeed #-}
lcgSeed :: IORef Int
lcgSeed = unsafePerformIO (newIORef 1442695040888963407)

-- | Generate next value and pick from a list.
lcgPick :: LCG -> [a] -> (a, LCG)
lcgPick (LCG s) xs =
  let s' = s * 6364136223846793005 + 1442695040888963407
      idx = abs (s' `div` 65536) `mod` length xs
  in (xs !! idx, LCG s')

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Autotune tile parameters with default configuration.
-- Results are cached by @(archName, n, k)@.
autotune :: Arch -> Int -> Int -> IO Tiles
autotune = autotuneWithConfig defaultAutotuneConfig

-- | Autotune tile parameters with explicit configuration.
autotuneWithConfig :: AutotuneConfig -> Arch -> Int -> Int -> IO Tiles
autotuneWithConfig cfg arch n k = do
  let key = (archName arch, n, k)
  cached <- lookupCache key
  case cached of
    Just tiles -> do
      when (atVerbose cfg) $
        printf "    [autotune] cache hit for %s n=%d k=%d → nAccum=%d tileK=%d\n"
          (archName arch) n k (nAccum tiles) (tileK tiles)
      return tiles
    Nothing -> do
      tiles <- graspSearch cfg arch n k
      insertCache key tiles
      return tiles
  where
    when True  m = m
    when False _ = return ()

-- ---------------------------------------------------------------------------
-- GRASP search
-- ---------------------------------------------------------------------------

graspSearch :: AutotuneConfig -> Arch -> Int -> Int -> IO Tiles
graspSearch cfg arch n k = do
  let kBlocks = k `div` 32
      candidates = enumerateCandidates arch n k

  when (atVerbose cfg) $ do
    printf "    [autotune] %s n=%d k=%d: %d candidates\n"
      (archName arch) n k (length candidates)
    hFlush stdout

  case candidates of
    []  -> do
      -- Fallback to heuristic if no candidates (shouldn't happen)
      let fallback = archTiles arch n k
      when (atVerbose cfg) $
        putStrLn "    [autotune] no candidates, falling back to heuristic"
      return fallback
    [c] -> do
      when (atVerbose cfg) $
        printf "    [autotune] single candidate: nAccum=%d tileK=%d\n"
          (nAccum (candTiles c)) (tileK (candTiles c))
      return (candTiles c)
    _   -> do
      rng <- lcgFromTime
      globalBest <- graspIterations cfg arch n kBlocks candidates rng Nothing
      let winner = brTiles globalBest
      when (atVerbose cfg) $
        printf "    [autotune] winner: nAccum=%d tileJ=%d tileK=%d (%.1f µs)\n"
          (nAccum winner) (tileJ winner) (tileK winner)
          (brMedianNs globalBest / 1000.0)
      return winner
  where
    when True  m = m
    when False _ = return ()

graspIterations :: AutotuneConfig -> Arch -> Int -> Int
                -> [Candidate] -> LCG -> Maybe BenchResult -> IO BenchResult
graspIterations cfg arch n kBlocks candidates rng0 mBest = go 0 rng0 mBest
  where
    go i rng best
      | i >= atIterations cfg = case best of
          Just b  -> return b
          Nothing -> error "[autotune] no iterations completed"
      | otherwise = do
          -- Construction phase: build RCL, pick randomly
          let sorted = sortBy (flip (comparing candScore)) candidates
              bestScore = candScore (head sorted)
              threshold = bestScore * atAlpha cfg
              rcl = filter (\c -> candScore c >= threshold) sorted
              (chosen, rng') = lcgPick rng rcl

          -- Benchmark the constructed solution
          result <- benchCandidate arch (candTiles chosen) n kBlocks

          when (atVerbose cfg) $
            printf "      iter %d: construct nAccum=%d tileK=%d → %.1f µs\n"
              (i + 1) (nAccum (brTiles result)) (tileK (brTiles result))
              (brMedianNs result / 1000.0)

          -- Local search phase
          improved <- localSearch cfg arch n kBlocks result

          when (atVerbose cfg && brTiles improved /= brTiles result) $
            printf "      iter %d: local search → nAccum=%d tileK=%d → %.1f µs\n"
              (i + 1) (nAccum (brTiles improved)) (tileK (brTiles improved))
              (brMedianNs improved / 1000.0)

          -- Update global best
          let best' = case best of
                Nothing -> Just improved
                Just b  -> Just (if brMedianNs improved < brMedianNs b
                                 then improved else b)
          go (i + 1) rng' best'

    when True  m = m
    when False _ = return ()

localSearch :: AutotuneConfig -> Arch -> Int -> Int -> BenchResult -> IO BenchResult
localSearch cfg arch n kBlocks current = do
  let k = kBlocks * 32
      nbrs = neighbors arch n k (brTiles current)
  case nbrs of
    [] -> return current
    _  -> do
      results <- mapM (\t -> benchCandidate arch t n kBlocks) nbrs
      let best = minimumBy (comparing brMedianNs) (current : results)
      if brTiles best /= brTiles current
        then localSearch cfg arch n kBlocks best  -- keep improving
        else return current                       -- converged
