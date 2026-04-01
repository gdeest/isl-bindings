-- | Architecture parameters for kernel generation.
--
-- An 'Arch' describes the target hardware's SIMD capabilities, cache
-- hierarchy, and core count. Tile sizes for the panel-packed microkernel
-- are derived from these parameters, ensuring portable performance
-- across ISAs.
--
-- @
-- -- Derive tile sizes for Zen 5:
-- let tiles = archTiles zen5 4096 128
-- -- tileJ = 64 (4 zmm accumulators × 16 floats)
-- -- tileK = 128 (panel fits L1)
-- @
module Isl.Infer.Arch
  ( -- * Architecture descriptor
    Arch(..)
  , Tiles(..)
    -- * Tile size derivation
  , archTiles
    -- * Architecture presets
  , zen5
  , avx2Generic
  , neonGeneric
  , archFromSimdWidth
  ) where

-- | Hardware architecture parameters.
--
-- These drive tile size selection and code generation strategy.
-- All sizes are in the natural units (floats for SIMD, bytes for cache).
data Arch = Arch
  { archName      :: !String
  , archSimdWidth :: !Int
    -- ^ Floats per SIMD register (16 for AVX-512, 8 for AVX2, 4 for NEON/SSE)
  , archSimdRegs  :: !Int
    -- ^ Number of usable SIMD registers (32 for AVX-512, 16 for AVX2/NEON)
  , archL1Size    :: !Int
    -- ^ L1 data cache in bytes (typically 32768 or 49152)
  , archL2Size    :: !Int
    -- ^ L2 cache in bytes (typically 1048576)
  , archCores     :: !Int
    -- ^ Physical cores available for OpenMP parallelism
  } deriving (Show)

-- | Derived tile sizes for the panel-packed microkernel.
data Tiles = Tiles
  { tileJ  :: !Int
    -- ^ Output elements per j-tile (multiple of archSimdWidth).
    -- Determines how many zmm accumulators the microkernel uses.
  , tileK  :: !Int
    -- ^ Float elements per k-tile.
    -- Panel size = tileJ × tileK × 4 bytes, must fit in L1.
  , nAccum :: !Int
    -- ^ Number of SIMD-width accumulators (= tileJ / archSimdWidth).
  } deriving (Show, Eq)

-- | Derive tile sizes from architecture parameters and matrix dimensions.
--
-- Strategy: balance panel size against L1 capacity.
--   1. Start with nAccum = L1 / (K * 4 * archSimdWidth) accumulator registers,
--      clamped to [2, archSimdRegs/3]. More accumulators = wider tileJ = more
--      ILP in the microkernel, but larger panel.
--   2. tileJ = nAccum × archSimdWidth
--   3. tileK = largest multiple of 32 (Q8 block boundary) such that
--      panel (tileJ × tileK × 4) + x slice (tileK × 4) fits in L1.
--   4. If tileK < 128, reduce nAccum and retry (prefer deeper k-tiles
--      for better amortization of the dequant overhead).
archTiles :: Arch -> Int -> Int -> Tiles
archTiles arch _n k = Tiles
  { tileJ  = tj
  , tileK  = tk
  , nAccum = nacc
  }
  where
    l1 = archL1Size arch
    sw = archSimdWidth arch
    maxAcc = archSimdRegs arch `div` 3

    -- Try accumulator counts from maxAcc down to 2, pick first with tileK >= 128
    (nacc, tj, tk) = head
      [ (na, na * sw, tkVal)
      | na <- [maxAcc, maxAcc - 1 .. 2]
      , let tjVal = na * sw
            tkMax = l1 `div` ((tjVal + 1) * 4)
            tkRnd = (tkMax `div` 32) * 32
            tkVal = max 32 (min tkRnd k)
      , tkVal >= 128 || na == 2  -- accept small tileK only at minimum accumulators
      ]

-- ---------------------------------------------------------------------------
-- Architecture presets
-- ---------------------------------------------------------------------------

-- | AMD Zen 5 (Ryzen AI 9 365, EPYC 9005 series).
-- AVX-512 with 32 zmm registers, 32KB L1d, 1MB L2 per core.
zen5 :: Arch
zen5 = Arch
  { archName      = "zen5"
  , archSimdWidth = 16     -- 512-bit / 32-bit = 16 floats
  , archSimdRegs  = 32
  , archL1Size    = 32768  -- 32 KB
  , archL2Size    = 1048576
  , archCores     = 10
  }

-- | Generic AVX2 target (Intel Haswell+, AMD Zen 1-4).
-- 256-bit SIMD, 16 ymm registers.
avx2Generic :: Arch
avx2Generic = Arch
  { archName      = "avx2"
  , archSimdWidth = 8      -- 256-bit / 32-bit = 8 floats
  , archSimdRegs  = 16
  , archL1Size    = 32768
  , archL2Size    = 262144
  , archCores     = 4
  }

-- | Generic ARM NEON target (Apple M-series, Cortex-A76+).
-- 128-bit SIMD, 32 registers.
neonGeneric :: Arch
neonGeneric = Arch
  { archName      = "neon"
  , archSimdWidth = 4      -- 128-bit / 32-bit = 4 floats
  , archSimdRegs  = 32
  , archL1Size    = 65536  -- 64 KB (Apple M-series)
  , archL2Size    = 4194304
  , archCores     = 8
  }

-- | Quick constructor from SIMD width alone (uses conservative defaults).
archFromSimdWidth :: Int -> Arch
archFromSimdWidth w = Arch
  { archName      = "custom_" ++ show (w * 32) ++ "bit"
  , archSimdWidth = w
  , archSimdRegs  = if w >= 16 then 32 else 16
  , archL1Size    = 32768
  , archL2Size    = 1048576
  , archCores     = 4
  }
