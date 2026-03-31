{-# LANGUAGE ForeignFunctionInterface #-}

-- | Panel-packed Q8_0 matvec kernel with architecture-parameterized tiling.
--
-- Strategy: per-tile dequantization + broadcast-FMA microkernel.
--
--   1. Outer parallel loop over j-tiles (polyhedral, via scanner)
--   2. For each k-tile:
--      a. S1 (dequant): convert Q8 blocks to float panel buffer (L1-resident)
--      b. S2 (µkernel): broadcast x[k], FMA into SIMD-width accumulators
--
-- This achieves AVX-512 vectorization (zmm FMA) by making the j-dimension
-- innermost and contiguous in the panel buffer. The Q8 weight data streams
-- from RAM at original bandwidth (no memory increase).
--
-- The tile sizes (tileJ, tileK) are derived from 'Arch' parameters,
-- making the kernel portable across ISAs.
module Isl.Infer.Kernel.Packed
  ( -- * Kernel compilation
    CompiledPackedMatvec(..)
  , compilePackedMatvec
    -- * C code generation (exported for inspection/testing)
  , genPackedMatvec
  ) where

import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)

import Isl.Infer.Arch
import Isl.Infer.Runtime

-- ---------------------------------------------------------------------------
-- FFI type — same signature as unpacked matvec for drop-in replacement
-- ---------------------------------------------------------------------------

type MatvecFnC = Ptr Float -> Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> IO ()

foreign import ccall "dynamic"
  mkMatvecFn :: FunPtr MatvecFnC -> MatvecFnC

-- | A compiled panel-packed matvec kernel.
data CompiledPackedMatvec = CompiledPackedMatvec
  { cpmFn     :: !MatvecFnC
  , cpmArch   :: !Arch
  , cpmTiles  :: !Tiles
  , cpmN      :: !Int
  , cpmKB     :: !Int
  , cpmKernel :: !CompiledKernel
  , cpmSource :: !String
  }

-- | Compile a panel-packed matvec for specific dimensions.
compilePackedMatvec :: Arch -> Int -> Int -> IO CompiledPackedMatvec
compilePackedMatvec arch n kBlocks = do
  let k      = kBlocks * 32
      tiles  = archTiles arch n k
      fname  = "q8mv_packed_" ++ archName arch
      src    = genPackedMatvec arch tiles n kBlocks
  ck <- compileAndLoad fname src
  let fn = mkMatvecFn (castFunPtr (ckFuncPtr ck))
  return CompiledPackedMatvec
    { cpmFn     = fn
    , cpmArch   = arch
    , cpmTiles  = tiles
    , cpmN      = n
    , cpmKB     = kBlocks
    , cpmKernel = ck
    , cpmSource = src
    }

-- ---------------------------------------------------------------------------
-- C code generation
-- ---------------------------------------------------------------------------

-- | Generate complete C source for the panel-packed matvec.
--
-- Structure:
--   - q8Typedefs (block_q8_0, f16_to_f32)
--   - Arch-derived constants as #defines
--   - void q8mv_packed_<arch>(...):
--       #pragma omp parallel for
--       for (tj = 0; tj < N; tj += TJ):
--         float acc[TJ] = {0}
--         for (tkb = 0; tkb < KB; tkb += TKB):
--           S1: dequant tile → panel[TJ × TKB*32]
--           S2: broadcast-FMA µkernel
--         store acc → out[tj..]
genPackedMatvec :: Arch -> Tiles -> Int -> Int -> String
genPackedMatvec arch tiles n kBlocks = unlines
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , ""
  , "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
  , "static inline float f16_to_f32(uint16_t h) {"
  , "    uint32_t sign = (uint32_t)(h >> 15) << 31;"
  , "    uint32_t exp  = (h >> 10) & 0x1F;"
  , "    uint32_t mant = h & 0x3FF;"
  , "    uint32_t f;"
  , "    if (exp == 0) {"
  , "        if (mant == 0) { f = sign; }"
  , "        else { exp = 1; while (!(mant & 0x400)) { mant <<= 1; exp--; }"
  , "               mant &= 0x3FF; f = sign | ((exp + 127 - 15) << 23) | (mant << 13); }"
  , "    } else if (exp == 31) { f = sign | 0x7F800000 | (mant << 13); }"
  , "    else { f = sign | ((exp + 127 - 15) << 23) | (mant << 13); }"
  , "    float result; __builtin_memcpy(&result, &f, 4); return result;"
  , "}"
  , ""
  , "/* Architecture: " ++ archName arch ++ " */"
  , "/* SIMD width: " ++ show (archSimdWidth arch) ++ " floats"
    ++ ", accumulators: " ++ show (nAccum tiles)
    ++ ", tileJ: " ++ show tj ++ ", tileK: " ++ show tk ++ " */"
  , "#define TJ " ++ show tj
  , "#define TKB " ++ show tkb
  , "#define TK " ++ show tk
  , "#define SIMD_W " ++ show (archSimdWidth arch)
  , "#define N_ACC " ++ show (nAccum tiles)
  , ""
  , "void " ++ funcName ++ "("
  , "    float* restrict out,"
  , "    const float* restrict x,"
  , "    const void* restrict W,"
  , "    int64_t N,"
  , "    int64_t KB"
  , ") {"
  , "    const block_q8_0* W_q8 = (const block_q8_0*)W;"
  , "    const int64_t K = KB * 32;"
  , ""
  , "    #pragma omp parallel for schedule(static)"
  , "    for (int64_t tj = 0; tj < N; tj += TJ) {"
  , "        /* Tile dimensions with remainder handling */"
  , "        int64_t nj = TJ;"
  , "        if (tj + TJ > N) nj = N - tj;"
  , ""
  , "        /* Accumulators: nj floats, SIMD-aligned */"
  , "        float acc[TJ] __attribute__((aligned(64)));"
  , "        memset(acc, 0, nj * sizeof(float));"
  , ""
  , "        /* Panel buffer: TKB blocks × 32 values × nj rows */"
  , "        /* Layout: panel[k * TJ + dj] for contiguous j-access */"
  , "        float panel[TK * TJ] __attribute__((aligned(64)));"
  , ""
  , "        for (int64_t tkb = 0; tkb < KB; tkb += TKB) {"
  , "            int64_t kb_end = tkb + TKB;"
  , "            if (kb_end > KB) kb_end = KB;"
  , "            int64_t nkb = kb_end - tkb;"
  , "            int64_t nk = nkb * 32;"
  , ""
  , "            /* S1: Dequantize + transpose tile into panel layout."
  , "               For each row dj in [0, nj), for each block in [tkb, kb_end):"
  , "               panel[k*TJ + dj] = scale(j,kb) * (float)qs[v]"
  , "               where k = (kb-tkb)*32 + v */"
  , "            for (int64_t dj = 0; dj < nj; dj++) {"
  , "                int64_t j = tj + dj;"
  , "                for (int64_t kb = tkb; kb < kb_end; kb++) {"
  , "                    const block_q8_0* blk = &W_q8[j * KB + kb];"
  , "                    float scale = f16_to_f32(blk->d);"
  , "                    int64_t base = (kb - tkb) * 32;"
  , "                    for (int v = 0; v < 32; v++) {"
  , "                        panel[(base + v) * TJ + dj] = scale * (float)blk->qs[v];"
  , "                    }"
  , "                }"
  , "            }"
  , ""
  , "            /* S2: Broadcast-FMA microkernel."
  , "               For each k: broadcast x[k], FMA into TJ accumulators."
  , "               The j-dimension is innermost → SIMD across j (zmm). */"
  , "            for (int64_t k = 0; k < nk; k++) {"
  , "                float xk = x[(tkb * 32) + k];"
  , "                const float* pw = &panel[k * TJ];"
  , "                #pragma omp simd"
  , "                for (int64_t dj = 0; dj < nj; dj++) {"
  , "                    acc[dj] += xk * pw[dj];"
  , "                }"
  , "            }"
  , "        }"
  , ""
  , "        /* Store accumulated results */"
  , "        memcpy(&out[tj], acc, nj * sizeof(float));"
  , "    }"
  , "}"
  ]
  where
    funcName = "q8mv_packed_" ++ archName arch
    tj  = tileJ tiles
    tk  = tileK tiles
    -- TKB: number of Q8 blocks per k-tile (tileK is in float elements)
    tkb = tk `div` 32
