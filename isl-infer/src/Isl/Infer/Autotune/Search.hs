-- | Search space enumeration and heuristic scoring for tile autotuning.
--
-- Enumerates all feasible @(nAccum, tileK)@ combinations for a given
-- architecture and matrix shape, scores them with a greedy heuristic
-- (L1 utilization × ILP factor), and provides local-search neighbors.
module Isl.Infer.Autotune.Search
  ( Candidate(..)
  , enumerateCandidates
  , neighbors
  ) where

import Isl.Infer.Arch

-- | A candidate tile configuration with its greedy heuristic score.
data Candidate = Candidate
  { candTiles :: !Tiles
  , candScore :: !Double
    -- ^ Greedy heuristic: L1 utilization × √nAccum. Higher = better predicted.
  } deriving (Show)

-- | Enumerate all feasible tile configurations for the given architecture
-- and matrix dimensions.
--
-- Search space:
--   nAccum ∈ [2 .. archSimdRegs/3]
--   tileK  ∈ {32, 64, ..., min(K, L1/(tileJ*4))}
--   tileJ  = nAccum × archSimdWidth
--
-- Feasibility: panel + x_slice must fit L1:
--   (tileJ + 1) × tileK × 4 ≤ archL1Size
enumerateCandidates :: Arch -> Int -> Int -> [Candidate]
enumerateCandidates arch n k =
  [ Candidate tiles (greedyScore arch tiles)
  | na <- [2 .. maxAcc]
  , let tjVal = na * sw
  , tjVal <= n  -- tileJ must not exceed output dimension
  , let tkMax = l1 `div` ((tjVal + 1) * 4)
  , tkMax >= 32  -- at least one k-tile must fit
  , tk <- [32, 64 .. min (min tkMax k) maxTK]
  , let tiles = Tiles { tileJ = tjVal, tileK = tk, nAccum = na }
  ]
  where
    l1     = archL1Size arch
    sw     = archSimdWidth arch
    maxAcc = archSimdRegs arch `div` 3
    maxTK  = 1024  -- cap to avoid pointless large tiles

-- | Greedy heuristic score for a tile configuration.
--
-- L1 utilization: fraction of L1 used by panel + x_slice (higher → less waste).
-- ILP factor: √nAccum (more accumulators → more instruction-level parallelism).
greedyScore :: Arch -> Tiles -> Double
greedyScore arch tiles =
  utilization * ilpFactor
  where
    panelBytes = (tileJ tiles + 1) * tileK tiles * 4
    utilization = fromIntegral panelBytes / fromIntegral (archL1Size arch)
    ilpFactor = sqrt (fromIntegral (nAccum tiles))

-- | Local search neighbors: ±1 on nAccum, ±32 on tileK.
-- Returns only feasible configurations.
neighbors :: Arch -> Int -> Int -> Tiles -> [Tiles]
neighbors arch n k tiles =
  [ t
  | (dna, dtk) <- [(-1,0), (1,0), (0,-32), (0,32)]
  , let na' = nAccum tiles + dna
        tk' = tileK tiles + dtk
  , na' >= 2, na' <= maxAcc
  , tk' >= 32, tk' <= k
  , let tj' = na' * archSimdWidth arch
  , tj' <= n
  , (tj' + 1) * tk' * 4 <= archL1Size arch
  , let t = Tiles { tileJ = tj', tileK = tk', nAccum = na' }
  , t /= tiles  -- exclude self
  ]
  where
    maxAcc = archSimdRegs arch `div` 3
