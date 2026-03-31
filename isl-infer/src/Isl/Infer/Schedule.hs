-- | Schedule definitions for polyhedral kernel compilation.
--
-- A 'Schedule' describes how to tile, parallelize, and vectorize
-- a polyhedral kernel. Schedules are verified at generation time
-- (when dimensions are known) using ISL, then compiled to native code.
--
-- Users modify schedules to explore the optimization space:
--
-- @
-- mySchedule = Schedule
--   { schTileJ  = 128    -- outer tile for parallelism
--   , schTileK  = 64     -- K-tile for cache reuse
--   , schParallel = True  -- OpenMP on outer j tiles
--   , schSimd    = True   -- #pragma omp simd on inner loop
--   }
-- @
module Isl.Infer.Schedule
  ( -- * Schedule types
    MatvecSchedule(..)
  , defaultSchedule
  , naiveSchedule
    -- * Presets
  , scheduleSmall     -- for small matrices (< 256)
  , scheduleMedium    -- for medium (256-2048)
  , scheduleLarge     -- for large (> 2048)
  ) where

-- | Schedule for Q8_0 matrix-vector multiply.
--
-- The base domain is @{ [j, kb] : 0 <= j < N, 0 <= kb < K/32 }@
-- where j is the output dimension and kb is the K-block dimension.
--
-- Tiling wraps this into:
-- @{ [tj, j, kb] : ... }@ (single-level) or
-- @{ [tj2, tj1, tkb, j, kb] : ... }@ (two-level)
data MatvecSchedule = MatvecSchedule
  { schName       :: !String
    -- Tiling
  , schTileJ      :: !Int      -- ^ Tile size for j (output rows). 0 = no tiling.
  , schTileJ2     :: !Int      -- ^ Outer tile for j (two-level). 0 = single-level.
  , schTileK      :: !Int      -- ^ Tile size for kb (K blocks). 0 = no K-tiling.
    -- Parallelism
  , schParallel   :: !Bool     -- ^ OpenMP parallel on outermost j tile
    -- Vectorization
  , schSimd       :: !Bool     -- ^ #pragma omp simd on innermost loop
  } deriving (Show)

-- | Default schedule: single-level tiling, parallel, SIMD.
-- Good for most LLM weight matrix sizes (4096×4096).
defaultSchedule :: MatvecSchedule
defaultSchedule = MatvecSchedule
  { schName     = "default"
  , schTileJ    = 128
  , schTileJ2   = 0
  , schTileK    = 0
  , schParallel = True
  , schSimd     = True
  }

-- | No tiling, no parallelism. Single-threaded baseline.
naiveSchedule :: MatvecSchedule
naiveSchedule = MatvecSchedule
  { schName     = "naive"
  , schTileJ    = 0
  , schTileJ2   = 0
  , schTileK    = 0
  , schParallel = False
  , schSimd     = False
  }

-- | For small matrices (e.g. KV projections: 4096→1024).
-- No parallelism (overhead dominates), just SIMD.
scheduleSmall :: MatvecSchedule
scheduleSmall = MatvecSchedule
  { schName = "small", schTileJ = 0, schTileJ2 = 0, schTileK = 0
  , schParallel = False, schSimd = True }

-- | For medium matrices (e.g. Q/O projections: 4096→4096).
scheduleMedium :: MatvecSchedule
scheduleMedium = MatvecSchedule
  { schName = "medium", schTileJ = 64, schTileJ2 = 0, schTileK = 0
  , schParallel = True, schSimd = True }

-- | For large matrices (e.g. FFN: 4096→14336).
scheduleLarge :: MatvecSchedule
scheduleLarge = MatvecSchedule
  { schName = "large", schTileJ = 192, schTileJ2 = 0, schTileK = 4
  , schParallel = True, schSimd = True }
