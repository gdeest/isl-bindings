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
  , MatvecStrategy(..)
    -- * KV cache
  , KVQuant(..)
  , KVCacheMode(..)
  , kvFloat32, kvQ8, kvQ4, kvK8V4, kvK4V8
    -- * Speculative decoding
  , SpecMode(..)
  , defaultSchedule
  , naiveSchedule
    -- * Presets
  , scheduleSmall     -- for small matrices (< 256)
  , scheduleMedium    -- for medium (256-2048)
  , scheduleLarge     -- for large (> 2048)
  ) where

-- | Strategy selector: original (polyhedral scanner) or packed (panel microkernel).
--
-- This is the top-level switch for A/B benchmarking different kernel strategies
-- on the same schedule parameters.
data MatvecStrategy
  = StrategyOriginal
    -- ^ Original Q8 body: 32-element inner dot product, vectorized by GCC (ymm).
    -- Uses the Boulet-Feautrier scanner for loop structure.
  | StrategyPacked
    -- ^ Panel-packed microkernel: per-tile dequant + broadcast-FMA.
    -- SIMD across j-dimension (zmm). Arch-parameterized tile sizes (heuristic).
  | StrategyPackedPoly
    -- ^ Like 'StrategyPacked' but with polyhedral loop generation.
    -- Loop nests are ISL-generated from domain constraints; only statement
    -- bodies (dequant, FMA) are hand-written. Gated for A/B testing.
  | StrategyAutotuned
    -- ^ Like 'StrategyPacked' but with GRASP-autotuned tile sizes.
    -- Empirically searches ~50 candidates at model load time (~5s per shape).
  deriving (Show, Eq)

-- | Quantization level for a single KV cache tensor.
data KVQuant
  = QFloat32   -- ^ Full float32 precision.
  | QQ8        -- ^ Q8_0 block quantization (4× compression, lossless).
  | QQ4        -- ^ Q4_0 block quantization (8× compression, <0.1 ppl loss).
  deriving (Show, Eq)

-- | KV cache configuration with independent K and V quantization.
--
-- All modes use transposed layout @[n_kv_heads, max_seq, head_dim]@.
-- K and V are quantized independently — KIVI (ICML'24) shows K needs
-- more precision than V since it drives attention scores.
--
-- Switchable at runtime via @/kvcache@ command for A/B benchmarking.
data KVCacheMode = KVCacheMode
  { kvKQuant :: !KVQuant   -- ^ K-cache quantization level.
  , kvVQuant :: !KVQuant   -- ^ V-cache quantization level (independent).
  } deriving (Show, Eq)

-- | Float32 K and V (baseline).
kvFloat32 :: KVCacheMode
kvFloat32 = KVCacheMode QFloat32 QFloat32

-- | Symmetric Q8 for K and V.
kvQ8 :: KVCacheMode
kvQ8 = KVCacheMode QQ8 QQ8

-- | Symmetric Q4 for K and V.
kvQ4 :: KVCacheMode
kvQ4 = KVCacheMode QQ4 QQ4

-- | K at Q8, V at Q4 — KIVI sweet spot.
kvK8V4 :: KVCacheMode
kvK8V4 = KVCacheMode QQ8 QQ4

-- | K at Q4, V at Q8.
kvK4V8 :: KVCacheMode
kvK4V8 = KVCacheMode QQ4 QQ8

-- | Speculative decoding mode.
--
-- N-gram speculative decode drafts K tokens from a bigram table built
-- from the generated text so far, then verifies the batch in one
-- @forwardBatch@ call.  Weight traffic is amortized K-fold — this is
-- the only optimization that changes the memory-bandwidth roofline.
data SpecMode
  = SpecOff           -- ^ No speculation (baseline sequential decode).
  | SpecNgram !Int    -- ^ N-gram draft with K lookahead tokens.
  | SpecDraft !Int    -- ^ Draft model with K lookahead tokens.
  deriving (Show, Eq)

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
    -- VNNI
  , schVnni       :: !Bool     -- ^ Use int8×int8→int32 VNNI accumulation (quantizes activations)
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
  , schVnni     = True   -- VNNI: 2× speedup via int8×int8→int32 accumulation
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
  , schVnni     = False
  }

-- | For small matrices (e.g. KV projections: 4096→1024).
-- No parallelism (overhead dominates), just SIMD.
scheduleSmall :: MatvecSchedule
scheduleSmall = MatvecSchedule
  { schName = "small", schTileJ = 0, schTileJ2 = 0, schTileK = 0
  , schParallel = False, schSimd = True, schVnni = False }

-- | For medium matrices (e.g. Q/O projections: 4096→4096).
scheduleMedium :: MatvecSchedule
scheduleMedium = MatvecSchedule
  { schName = "medium", schTileJ = 64, schTileJ2 = 0, schTileK = 0
  , schParallel = True, schSimd = True, schVnni = False }

-- | For large matrices (e.g. FFN: 4096→14336).
scheduleLarge :: MatvecSchedule
scheduleLarge = MatvecSchedule
  { schName = "large", schTileJ = 192, schTileJ2 = 0, schTileK = 4
  , schParallel = True, schSimd = True, schVnni = False }
