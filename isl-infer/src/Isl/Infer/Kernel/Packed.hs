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
  , compilePackedMatvecWith
  , compilePackedMatvecQ
  , compilePackedMatvecWithQ
    -- * Packed GEMM (batched matvec with weight reuse)
  , CompiledPackedGemm(..)
  , compilePackedGemm
  , callPackedGemm
    -- * C code generation (exported for inspection/testing)
  , genPackedMatvec
  , genPackedMatvecQ
  , genPackedGemm
  ) where

import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)

import Isl.Infer.Arch
import Isl.Infer.Kernel.GEMM (WeightQuant(..))
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

-- | Compile a panel-packed matvec for specific dimensions (Q8_0).
compilePackedMatvec :: Arch -> Int -> Int -> IO CompiledPackedMatvec
compilePackedMatvec = compilePackedMatvecQ WQ8

-- | Like 'compilePackedMatvec' but with explicit tile parameters.
-- Used by the autotuner to compile candidate tile configurations.
compilePackedMatvecWith :: Arch -> Tiles -> Int -> Int -> IO CompiledPackedMatvec
compilePackedMatvecWith = compilePackedMatvecWithQ WQ8

-- | Compile a panel-packed matvec with chosen weight quantization.
compilePackedMatvecQ :: WeightQuant -> Arch -> Int -> Int -> IO CompiledPackedMatvec
compilePackedMatvecQ wq arch n kBlocks =
  compilePackedMatvecWithQ wq arch (archTiles arch n (kBlocks * 32)) n kBlocks

-- | Like 'compilePackedMatvecQ' but with explicit tile parameters.
compilePackedMatvecWithQ :: WeightQuant -> Arch -> Tiles -> Int -> Int -> IO CompiledPackedMatvec
compilePackedMatvecWithQ wq arch tiles n kBlocks = do
  let prefix = case wq of WQ8 -> "q8mv_packed_"; WQ4 -> "q4mv_packed_"; WQ4K -> "q4kmv_packed_"
      fname  = prefix ++ archName arch
      -- Standalone packed kernel always uses float path (VNNI is in the fused layer)
      src    = genPackedMatvecQ wq arch tiles n kBlocks
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
genPackedMatvec = genPackedMatvecQ WQ8

-- | Generate complete C source for a panel-packed matvec with chosen weight quant.
--
-- Only S1 (dequant) differs between Q8 and Q4. S2 (broadcast-FMA) is identical
-- since it operates on the post-dequant float panel buffer.
genPackedMatvecQ :: WeightQuant -> Arch -> Tiles -> Int -> Int -> String
genPackedMatvecQ wq arch tiles n kBlocks = unlines $
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , ""
  ] ++ packedTypedefs wq ++
  [ ""
  , "/* Architecture: " ++ archName arch ++ ", quant: " ++ show wq ++ " */"
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
  ] ++ packedWCast wq ++
  [ "    const int64_t K = KB * 32;"
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
  ] ++ packedS1 wq ++
  [ ""
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
    prefix = case wq of WQ8 -> "q8mv_packed_"; WQ4 -> "q4mv_packed_"; WQ4K -> "q4kmv_packed_"
    funcName = prefix ++ archName arch
    tj  = tileJ tiles
    tk  = tileK tiles
    -- TKB: number of Q8/Q4 blocks per k-tile (tileK is in float elements)
    tkb = tk `div` 32

-- | Block type definition and f16 helper, per weight quant.
packedTypedefs :: WeightQuant -> [String]
packedTypedefs WQ8 =
  [ "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
  , f16Helper
  ]
packedTypedefs WQ4 =
  [ "typedef struct { uint16_t d; uint8_t qs[16]; } block_q4_0;"
  , f16Helper
  ]
packedTypedefs WQ4K =
  [ "typedef struct {"
  , "    uint16_t d; uint16_t dmin;"
  , "    uint8_t scales[12]; uint8_t qs[128];"
  , "} block_q4_K;"
  , f16Helper
  ]

-- | Weight pointer cast at function entry.
packedWCast :: WeightQuant -> [String]
packedWCast WQ8  = ["    const block_q8_0* W_q8 = (const block_q8_0*)W;"]
packedWCast WQ4  = ["    const block_q4_0* W_q4 = (const block_q4_0*)W;"]
packedWCast WQ4K = ["    const block_q4_K* W_q4k = (const block_q4_K*)W;"]

-- | S1: dequantize + transpose tile into panel layout.
-- Only this stage differs between Q8 and Q4. The output (float panel) is identical.
packedS1 :: WeightQuant -> [String]
packedS1 WQ8 =
  [ "            /* S1: Dequantize Q8_0 + transpose tile into panel layout. */"
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
  ]
packedS1 WQ4 =
  [ "            /* S1: Dequantize Q4_0 + transpose tile into panel layout."
  , "               Each byte packs two 4-bit values (low nibble first), biased by -8. */"
  , "            for (int64_t dj = 0; dj < nj; dj++) {"
  , "                int64_t j = tj + dj;"
  , "                for (int64_t kb = tkb; kb < kb_end; kb++) {"
  , "                    const block_q4_0* blk = &W_q4[j * KB + kb];"
  , "                    float scale = f16_to_f32(blk->d);"
  , "                    int64_t base = (kb - tkb) * 32;"
  , "                    for (int v = 0; v < 32; v++) {"
  , "                        int nibble = (blk->qs[v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "                        panel[(base + v) * TJ + dj] = scale * (float)(nibble - 8);"
  , "                    }"
  , "                }"
  , "            }"
  ]
packedS1 WQ4K =
  [ "            /* S1: Dequantize Q4_K super-blocks + transpose into panel layout. */"
  , "            for (int64_t dj = 0; dj < nj; dj++) {"
  , "                int64_t j = tj + dj;"
  , "                for (int64_t kb = tkb; kb < kb_end; kb++) {"
  , "                    int64_t sb = kb / 8; int64_t si = kb & 7;"
  , "                    const block_q4_K* sblk = &W_q4k[j * (KB / 8) + sb];"
  , "                    uint8_t utmp0, utmp1;"
  , "                    if (si < 4) {"
  , "                        utmp0 = sblk->scales[si] & 63;"
  , "                        utmp1 = sblk->scales[si + 4] & 63;"
  , "                    } else {"
  , "                        utmp0 = (sblk->scales[si+4]&0xF)|((sblk->scales[si-4]>>6)<<4);"
  , "                        utmp1 = (sblk->scales[si+4]>>4)|((sblk->scales[si]>>6)<<4);"
  , "                    }"
  , "                    float d_sc = f16_to_f32(sblk->d) * utmp0;"
  , "                    float d_mn = f16_to_f32(sblk->dmin) * utmp1;"
  , "                    int64_t qoff = si * 16;"
  , "                    int64_t base = (kb - tkb) * 32;"
  , "                    for (int v = 0; v < 32; v++) {"
  , "                        int nibble = (sblk->qs[qoff + v/2] >> ((v&1)*4)) & 0xF;"
  , "                        panel[(base + v) * TJ + dj] = d_sc * (float)nibble - d_mn;"
  , "                    }"
  , "                }"
  , "            }"
  ]

-- ---------------------------------------------------------------------------
-- Packed GEMM: batched matvec with weight panel reuse across B tokens
-- ---------------------------------------------------------------------------

-- | Y[b,j] = W[j,:] @ X[b,:] for b in [0..B), j in [0..N)
-- Same panel-packed strategy as matvec but with B input vectors sharing
-- the dequanted weight panel. Weight traffic = 1× (same as single matvec),
-- compute = B×. This is the key to fast speculative decode verification.
type GemmFnC = Ptr Float -> Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> Int64 -> IO ()

foreign import ccall "dynamic"
  mkGemmFn :: FunPtr GemmFnC -> GemmFnC

data CompiledPackedGemm = CompiledPackedGemm
  { cpgFn     :: !GemmFnC
  , cpgArch   :: !Arch
  , cpgTiles  :: !Tiles
  , cpgN      :: !Int
  , cpgKB     :: !Int
  , cpgKernel :: !CompiledKernel
  , cpgSource :: !String
  }

-- | Compile a packed GEMM kernel for specific dimensions (Q8_0).
compilePackedGemm :: Arch -> Int -> Int -> IO CompiledPackedGemm
compilePackedGemm arch n kBlocks = do
  let tiles = archTiles arch n (kBlocks * 32)
      fname = "q8gemm_packed_" ++ archName arch
      src   = genPackedGemm arch tiles n kBlocks
  ck <- compileAndLoad fname src
  let fn = mkGemmFn (castFunPtr (ckFuncPtr ck))
  return CompiledPackedGemm
    { cpgFn = fn, cpgArch = arch, cpgTiles = tiles
    , cpgN = n, cpgKB = kBlocks, cpgKernel = ck, cpgSource = src }

-- | Call a compiled packed GEMM.
callPackedGemm :: CompiledPackedGemm -> Ptr Float -> Ptr Float -> Ptr Word8 -> Int -> IO ()
callPackedGemm cpg yPtr xPtr wPtr batchSize =
  cpgFn cpg yPtr xPtr wPtr
    (fromIntegral batchSize) (fromIntegral (cpgN cpg)) (fromIntegral (cpgKB cpg))

-- | Generate C source for a packed GEMM kernel.
--
-- Structure: same as packed matvec but S2 loops over B input vectors,
-- sharing the dequanted weight panel. Each batch token has its own
-- accumulator array.
--
-- Memory: weight panel in L1 (shared), B × TJ accumulators on stack.
-- For B=8, TJ=64: 8 × 64 × 4 = 2KB accumulators. Fits easily.
genPackedGemm :: Arch -> Tiles -> Int -> Int -> String
genPackedGemm arch tiles n kBlocks = unlines $
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , ""
  ] ++ packedTypedefs WQ8 ++
  [ ""
  , "/* Packed GEMM: Y[b,j] = W[j,:] @ X[b,:] */"
  , "/* Arch: " ++ archName arch ++ ", tileJ=" ++ show tj ++ ", tileK=" ++ show tk ++ " */"
  , "#define TJ " ++ show tj
  , "#define TKB " ++ show tkb
  , "#define TK " ++ show tk
  , "#define B_MAX 128"
  , ""
  , "void q8gemm_packed_" ++ archName arch ++ "("
  , "    float* restrict Y,"      -- [B, N]
  , "    const float* restrict X," -- [B, K]
  , "    const void* restrict W,"  -- [N, KB] Q8_0 blocks
  , "    int64_t B,"
  , "    int64_t N,"
  , "    int64_t KB"
  , ") {"
  , "    const block_q8_0* W_q8 = (const block_q8_0*)W;"
  , "    const int64_t K = KB * 32;"
  , ""
  , "    /* Zero output — this kernel accumulates */"
  , "    memset(Y, 0, B * N * sizeof(float));"
  , ""
  , "    #pragma omp parallel for schedule(static)"
  , "    for (int64_t tj = 0; tj < N; tj += TJ) {"
  , "        int64_t nj = TJ;"
  , "        if (tj + TJ > N) nj = N - tj;"
  , ""
  , "        /* Per-batch accumulators on stack */"
  , "        float acc[B_MAX][TJ] __attribute__((aligned(64)));"
  , "        for (int64_t b = 0; b < B; b++)"
  , "            memset(acc[b], 0, nj * sizeof(float));"
  , ""
  , "        /* Panel buffer (weight tile, shared across B tokens) */"
  , "        float panel[TK * TJ] __attribute__((aligned(64)));"
  , ""
  , "        for (int64_t tkb = 0; tkb < KB; tkb += TKB) {"
  , "            int64_t kb_end = tkb + TKB;"
  , "            if (kb_end > KB) kb_end = KB;"
  , "            int64_t nkb = kb_end - tkb;"
  , "            int64_t nk = nkb * 32;"
  , ""
  , "            /* S1: Dequant Q8 blocks → panel[k*TJ + dj] */"
  , "            for (int64_t dj = 0; dj < nj; dj++) {"
  , "                int64_t j = tj + dj;"
  , "                for (int64_t kb = tkb; kb < kb_end; kb++) {"
  , "                    const block_q8_0* blk = &W_q8[j * KB + kb];"
  , "                    float scale = f16_to_f32(blk->d);"
  , "                    int64_t base = (kb - tkb) * 32;"
  , "                    for (int v = 0; v < 32; v++)"
  , "                        panel[(base + v) * TJ + dj] = scale * (float)blk->qs[v];"
  , "                }"
  , "            }"
  , ""
  , "            /* S2: Broadcast-FMA for each batch token */"
  , "            for (int64_t b = 0; b < B; b++) {"
  , "                for (int64_t k = 0; k < nk; k++) {"
  , "                    float xk = X[b * K + (tkb * 32) + k];"
  , "                    const float* pw = &panel[k * TJ];"
  , "                    #pragma omp simd"
  , "                    for (int64_t dj = 0; dj < nj; dj++) {"
  , "                        acc[b][dj] += xk * pw[dj];"
  , "                    }"
  , "                }"
  , "            }"
  , "        }"
  , ""
  , "        /* Store results */"
  , "        for (int64_t b = 0; b < B; b++)"
  , "            memcpy(&Y[b * N + tj], acc[b], nj * sizeof(float));"
  , "    }"
  , "}"
  ]
  where
    tj  = tileJ tiles
    tk  = tileK tiles
    tkb = tk `div` 32

-- | Shared f16-to-f32 conversion helper using hardware F16C instruction.
f16Helper :: String
f16Helper = unlines
  [ "#include <immintrin.h>"
  , "static inline float f16_to_f32(uint16_t h) {"
  , "    return _cvtsh_ss(h);"
  , "}"
  ]
