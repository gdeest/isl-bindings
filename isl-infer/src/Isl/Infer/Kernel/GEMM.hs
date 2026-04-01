{-# LANGUAGE ForeignFunctionInterface #-}

-- | Polyhedral Q8_0 matrix-vector and matrix-matrix kernels.
--
-- Defines iteration domains as parametric polyhedra,
-- applies schedule transforms, verifies at generation time via ISL,
-- specializes with concrete dimensions, scans, generates C, compiles.
--
-- == Matvec (decode)
-- The generated matvec kernel replaces the hand-written @q8_matvec@ from cbits.
-- Domain: @{ [j, kb] : 0 <= j < N, 0 <= kb < KB }@
--
-- == GEMM (prefill)
-- Batched Q8 projection: @Y[b,j] = Σ_kb dequant(W[j,kb]) · X[b,kb*32..+32]@
-- Domain: @{ [b, j, kb] : 0 <= b < B, 0 <= j < N, 0 <= kb < KB }@
-- With J-tiling: @{ [tj, b, j, kb] }@ (4D) or J+KB-tiling: @{ [tj, tkb, b, j, kb] }@ (5D).
module Isl.Infer.Kernel.GEMM
  ( -- * Weight quantization type
    WeightQuant(..)
    -- * Matvec compilation (decode)
  , CompiledMatvec(..)
  , compileMatvec
  , compileMatvecQ
    -- * GEMM compilation (prefill)
  , CompiledQ8Gemm(..)
  , GemmSchedule(..)
  , GemmBatchPos(..)
  , defaultGemmSchedule
  , compileQ8Gemm
  , compileGemmQ
  , compileGemmQV
  , callQ8Gemm
    -- * Polyhedron definitions (exported for inspection/testing)
  , matvecDomain
  , tiledMatvecDomain
  , twoLevelMatvecDomain
  , q8GemmDomain
  , tiledQ8GemmDomain
  , twoLevelQ8GemmDomain
  ) where

import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction, PDisjunction(..), mkPConjunction)
import Isl.Scan (mkScanner, Scanner(..))

import Isl.Scan.Types (LoopNest)
import Isl.Infer.Codegen
import Isl.Infer.Runtime
import Isl.Infer.Schedule
import Isl.Infer.Specialize

-- ---------------------------------------------------------------------------
-- FFI type for the generated matvec
-- ---------------------------------------------------------------------------

-- | C signature: void f(float* out, float* x, void* W, int64_t N, int64_t KB)
type MatvecFnC = Ptr Float -> Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> IO ()

foreign import ccall "dynamic"
  mkMatvecFn :: FunPtr MatvecFnC -> MatvecFnC

-- | A compiled polyhedral matvec kernel.
data CompiledMatvec = CompiledMatvec
  { cmFn       :: !MatvecFnC           -- ^ The callable function
  , cmSchedule :: !MatvecSchedule       -- ^ Schedule used to generate it
  , cmN        :: !Int                  -- ^ Output dimension it was specialized for
  , cmKBlocks  :: !Int                  -- ^ K/32 it was specialized for
  , cmKernel   :: !CompiledKernel       -- ^ Underlying .so handle
  , cmSource   :: !String               -- ^ Generated C source (for inspection)
  }

-- ---------------------------------------------------------------------------
-- Parametric polyhedra (for correctness reasoning)
-- ---------------------------------------------------------------------------

-- | Base matvec domain (no tiling): { [j, kb] : 0 <= j < N, 0 <= kb < KB }
-- Params: KB=0 (K blocks = K/32), N=1 (output rows)
-- Dims:   j=0, kb=1
matvecDomain :: PConjunction '["KB", "N"] 2
matvecDomain = mkPConjunction @'["KB","N"] @2 $
  \(kbp :- np :- Nil) (j :- kb :- Nil) ->
    idx j  >=: cst 0 &&: idx j  <=: idx np -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1

-- | Single-level tiled: { [tj, j, kb] : ... }
-- Dims: tj=0, j=1, kb=2
tiledMatvecDomain :: Integer -> PConjunction '["KB", "N"] 3
tiledMatvecDomain t = mkPConjunction @'["KB","N"] @3 $
  \(kbp :- np :- Nil) (tj :- j :- kb :- Nil) ->
        idx tj >=: cst 0 &&: t *: idx tj <=: idx np -: cst 1
    &&: idx j  >=: t *: idx tj &&: idx j <=: t *: idx tj +: cst (t - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1

-- | Two-level tiled: { [tj, tkb, j, kb] : ... }
-- Dims: tj=0, tkb=1, j=2, kb=3
twoLevelMatvecDomain :: Integer -> Integer -> PConjunction '["KB", "N"] 4
twoLevelMatvecDomain tj tk = mkPConjunction @'["KB","N"] @4 $
  \(kbp :- np :- Nil) (tji :- tkbi :- j :- kb :- Nil) ->
        idx tji  >=: cst 0 &&: tj *: idx tji <=: idx np -: cst 1
    &&: idx tkbi >=: cst 0 &&: tk *: idx tkbi <=: idx kbp -: cst 1
    &&: idx j  >=: tj *: idx tji &&: idx j <=: tj *: idx tji +: cst (tj - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx kb >=: tk *: idx tkbi &&: idx kb <=: tk *: idx tkbi +: cst (tk - 1)
    &&: idx kb <=: idx kbp -: cst 1

-- ---------------------------------------------------------------------------
-- Weight quantization type
-- ---------------------------------------------------------------------------

-- | Weight quantization format selector.
-- Both Q8 and Q4 use 32-element blocks (KB = K/32), so the polyhedral
-- domains and schedules are identical — only the innermost body differs.
data WeightQuant = WQ8 | WQ4 | WQ4K
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Q8_0 block dot product body template
-- ---------------------------------------------------------------------------

-- | The C body for Q8_0 matvec inner loop.
-- Variables available: j (output row), kb (K block index).
-- Arrays: out (float*), x (float*), W (block_q8_0*).
q8Body :: String
q8Body = unlines
  [ "{"
  , "    const block_q8_0* blk = &W_q8[j * KB + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    float block_sum = 0.0f;"
  , "    #pragma omp simd reduction(+:block_sum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        block_sum += x[kb * 32 + v] * (float)blk->qs[v];"
  , "    }"
  , "    out[j] += scale * block_sum;"
  , "}"
  ]

-- | C preamble: type definitions, accumulator init.
-- | Types and helpers — go before the function (in ckTypedefs).
q8Typedefs :: String
q8Typedefs = unlines
  [ "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
  , f16ToF32Def
  ]

q8Preamble :: String
q8Preamble = unlines
  [ "    const block_q8_0* W_q8 = (const block_q8_0*)W;"
  , "    for (int64_t jj = 0; jj < N; jj++) out[jj] = 0.0f;"
  ]

q8Postamble :: String
q8Postamble = ""

-- ---------------------------------------------------------------------------
-- VNNI int8×int8→int32 body templates
-- ---------------------------------------------------------------------------

-- | Q8_0 matvec with VNNI int8×int8→int32 accumulation.
-- The preamble quantizes the activation vector x to Q8_0 format (x_q8/x_d),
-- then the body accumulates in int32 and applies combined scales at the end.
-- This enables GCC to use AVX-512 VNNI instructions (vpdpbusd/vpdpbssd).
q8VnniBody :: String
q8VnniBody = unlines
  [ "{"
  , "    const block_q8_0* blk = &W_q8[j * KB + kb];"
  , "    int32_t isum = 0;"
  , "    #pragma omp simd reduction(+:isum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        isum += (int32_t)blk->qs[v] * (int32_t)x_q8[kb * 32 + v];"
  , "    }"
  , "    out[j] += f16_to_f32(blk->d) * x_d[kb] * (float)isum;"
  , "}"
  ]

-- | Preamble: quantize x to Q8_0 symmetric format (one scale per 32 elements).
q8VnniPreamble :: String
q8VnniPreamble = unlines
  [ "    const block_q8_0* W_q8 = (const block_q8_0*)W;"
  , "    for (int64_t jj = 0; jj < N; jj++) out[jj] = 0.0f;"
  , "    /* Quantize activation vector to Q8_0 for VNNI int8x8->int32 accumulation */"
  , "    int8_t x_q8[KB * 32];"
  , "    float x_d[KB];"
  , "    for (int64_t blk = 0; blk < KB; blk++) {"
  , "        float amax = 0.0f;"
  , "        for (int v = 0; v < 32; v++) {"
  , "            float av = fabsf(x[blk * 32 + v]);"
  , "            if (av > amax) amax = av;"
  , "        }"
  , "        x_d[blk] = amax / 127.0f;"
  , "        float id = amax > 0.0f ? 127.0f / amax : 0.0f;"
  , "        for (int v = 0; v < 32; v++)"
  , "            x_q8[blk * 32 + v] = (int8_t)roundf(x[blk * 32 + v] * id);"
  , "    }"
  ]

-- | Q4_0 matvec with VNNI int8×int8→int32 accumulation.
-- Nibbles are unpacked to int8 [-8..7] then multiplied with quantized x.
q4VnniBody :: String
q4VnniBody = unlines
  [ "{"
  , "    const block_q4_0* blk = &W_q4[j * KB + kb];"
  , "    int32_t isum = 0;"
  , "    #pragma omp simd reduction(+:isum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        int nibble = (blk->qs[v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "        isum += (int32_t)(int8_t)(nibble - 8) * (int32_t)x_q8[kb * 32 + v];"
  , "    }"
  , "    out[j] += f16_to_f32(blk->d) * x_d[kb] * (float)isum;"
  , "}"
  ]

q4VnniPreamble :: String
q4VnniPreamble = unlines
  [ "    const block_q4_0* W_q4 = (const block_q4_0*)W;"
  , "    for (int64_t jj = 0; jj < N; jj++) out[jj] = 0.0f;"
  , "    /* Quantize activation vector to Q8_0 for VNNI int8x8->int32 accumulation */"
  , "    int8_t x_q8[KB * 32];"
  , "    float x_d[KB];"
  , "    for (int64_t blk = 0; blk < KB; blk++) {"
  , "        float amax = 0.0f;"
  , "        for (int v = 0; v < 32; v++) {"
  , "            float av = fabsf(x[blk * 32 + v]);"
  , "            if (av > amax) amax = av;"
  , "        }"
  , "        x_d[blk] = amax / 127.0f;"
  , "        float id = amax > 0.0f ? 127.0f / amax : 0.0f;"
  , "        for (int v = 0; v < 32; v++)"
  , "            x_q8[blk * 32 + v] = (int8_t)roundf(x[blk * 32 + v] * id);"
  , "    }"
  ]

-- ---------------------------------------------------------------------------
-- Q4_K super-block body templates
-- ---------------------------------------------------------------------------

-- | Q4_K typedef: 256-element super-blocks with hierarchical 6-bit scales.
-- Super-block = d(f16) + dmin(f16) + scales[12] (6-bit packed) + qs[128] (4-bit packed).
-- 144 bytes per 256 elements.
--
-- The polyhedral domain is unchanged (KB = K/32, 32-element sub-blocks).
-- The body indexes super-blocks via kb/8, sub-blocks via kb%8.
q4kTypedefs :: String
q4kTypedefs = unlines
  [ "typedef struct {"
  , "    uint16_t d;        /* super-block scale */"
  , "    uint16_t dmin;     /* super-block min */"
  , "    uint8_t scales[12]; /* 6-bit sub-block scales+mins packed */"
  , "    uint8_t qs[128];    /* 4-bit quantized values */"
  , "} block_q4_K;"
  , f16ToF32Def
  ]

-- | Q4_K matvec body (float path).
-- Iteration: per 32-element sub-block within a 256-element super-block.
-- Dequant: w = d * sc * nibble - dmin * mn
q4kBody :: String
q4kBody = unlines
  [ "{"
  , "    int64_t sb = kb / 8;         /* super-block index */"
  , "    int64_t si = kb & 7;         /* sub-block within super-block */"
  , "    const block_q4_K* sblk = &W_q4k[j * (KB / 8) + sb];"
  , "    /* Unpack 6-bit scale and min for this sub-block */"
  , "    uint8_t utmp[2];"
  , "    if (si < 4) {"
  , "        utmp[0] = sblk->scales[si] & 63;"
  , "        utmp[1] = sblk->scales[si + 4] & 63;"
  , "    } else {"
  , "        utmp[0] = (sblk->scales[si + 4] & 0xF) | ((sblk->scales[si - 4] >> 6) << 4);"
  , "        utmp[1] = (sblk->scales[si + 4] >>  4) | ((sblk->scales[si]     >> 6) << 4);"
  , "    }"
  , "    float d_sc = f16_to_f32(sblk->d) * utmp[0];"
  , "    float d_mn = f16_to_f32(sblk->dmin) * utmp[1];"
  , "    int64_t qoff = si * 16;      /* byte offset into qs[] */"
  , "    float block_sum = 0.0f;"
  , "    #pragma omp simd reduction(+:block_sum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        int nibble = (sblk->qs[qoff + v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "        block_sum += x[kb * 32 + v] * (d_sc * (float)nibble - d_mn);"
  , "    }"
  , "    out[j] += block_sum;"
  , "}"
  ]

q4kPreamble :: String
q4kPreamble = unlines
  [ "    const block_q4_K* W_q4k = (const block_q4_K*)W;"
  , "    for (int64_t jj = 0; jj < N; jj++) out[jj] = 0.0f;"
  ]

-- | Q4_K VNNI body: uses pre-quantized x (int8×int8 for nibble part).
-- The min correction term needs float x, so we precompute per-block x sums.
q4kVnniBody :: String
q4kVnniBody = unlines
  [ "{"
  , "    int64_t sb = kb / 8;"
  , "    int64_t si = kb & 7;"
  , "    const block_q4_K* sblk = &W_q4k[j * (KB / 8) + sb];"
  , "    uint8_t utmp[2];"
  , "    if (si < 4) {"
  , "        utmp[0] = sblk->scales[si] & 63;"
  , "        utmp[1] = sblk->scales[si + 4] & 63;"
  , "    } else {"
  , "        utmp[0] = (sblk->scales[si + 4] & 0xF) | ((sblk->scales[si - 4] >> 6) << 4);"
  , "        utmp[1] = (sblk->scales[si + 4] >>  4) | ((sblk->scales[si]     >> 6) << 4);"
  , "    }"
  , "    float d_sc = f16_to_f32(sblk->d) * utmp[0];"
  , "    float d_mn = f16_to_f32(sblk->dmin) * utmp[1];"
  , "    int64_t qoff = si * 16;"
  , "    int32_t isum = 0;"
  , "    #pragma omp simd reduction(+:isum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        int nibble = (sblk->qs[qoff + v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "        isum += (int32_t)(int8_t)nibble * (int32_t)x_q8[kb * 32 + v];"
  , "    }"
  , "    out[j] += d_sc * x_d[kb] * (float)isum - d_mn * x_sums[kb];"
  , "}"
  ]

-- | VNNI preamble for Q4_K: also precomputes per-block x sums for min correction.
q4kVnniPreamble :: String
q4kVnniPreamble = unlines
  [ "    const block_q4_K* W_q4k = (const block_q4_K*)W;"
  , "    for (int64_t jj = 0; jj < N; jj++) out[jj] = 0.0f;"
  , "    /* Quantize activation vector to Q8_0 + precompute block sums for min correction */"
  , "    int8_t x_q8[KB * 32];"
  , "    float x_d[KB];"
  , "    float x_sums[KB];"
  , "    for (int64_t blk = 0; blk < KB; blk++) {"
  , "        float amax = 0.0f;"
  , "        float bsum = 0.0f;"
  , "        for (int v = 0; v < 32; v++) {"
  , "            float av = fabsf(x[blk * 32 + v]);"
  , "            if (av > amax) amax = av;"
  , "            bsum += x[blk * 32 + v];"
  , "        }"
  , "        x_d[blk] = amax / 127.0f;"
  , "        x_sums[blk] = bsum;"
  , "        float id = amax > 0.0f ? 127.0f / amax : 0.0f;"
  , "        for (int v = 0; v < 32; v++)"
  , "            x_q8[blk * 32 + v] = (int8_t)roundf(x[blk * 32 + v] * id);"
  , "    }"
  ]

-- ---------------------------------------------------------------------------
-- Q4_0 block dot product body template
-- ---------------------------------------------------------------------------

-- | The C body for Q4_0 matvec inner loop.
-- Same iteration domain as Q8 (KB = K/32, both use 32-element blocks).
-- Decoding: each byte packs two 4-bit values (low nibble first), biased by -8.
q4Body :: String
q4Body = unlines
  [ "{"
  , "    const block_q4_0* blk = &W_q4[j * KB + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    float block_sum = 0.0f;"
  , "    #pragma omp simd reduction(+:block_sum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        int nibble = (blk->qs[v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "        block_sum += x[kb * 32 + v] * (float)(nibble - 8);"
  , "    }"
  , "    out[j] += scale * block_sum;"
  , "}"
  ]

q4Typedefs :: String
q4Typedefs = unlines
  [ "typedef struct { uint16_t d; uint8_t qs[16]; } block_q4_0;"
  , f16ToF32Def
  ]

q4Preamble :: String
q4Preamble = unlines
  [ "    const block_q4_0* W_q4 = (const block_q4_0*)W;"
  , "    for (int64_t jj = 0; jj < N; jj++) out[jj] = 0.0f;"
  ]

q4Postamble :: String
q4Postamble = ""

-- | Shared f16-to-f32 conversion helper using hardware F16C instruction.
-- Requires -mf16c (already in cabal cc-options). Falls back to software on
-- platforms without F16C, but all x86-64-v3+ CPUs have it (Haswell 2013+).
f16ToF32Def :: String
f16ToF32Def = unlines
  [ "#include <immintrin.h>"
  , "static inline float f16_to_f32(uint16_t h) {"
  , "    return _cvtsh_ss(h);"
  , "}"
  ]

-- | Select body/typedefs/preamble based on weight quantization and VNNI mode.
bodyFor :: Bool -> WeightQuant -> String
bodyFor False WQ8  = q8Body
bodyFor False WQ4  = q4Body
bodyFor False WQ4K = q4kBody
bodyFor True  WQ8  = q8VnniBody
bodyFor True  WQ4  = q4VnniBody
bodyFor True  WQ4K = q4kVnniBody

typedefsFor :: WeightQuant -> String
typedefsFor WQ8  = q8Typedefs
typedefsFor WQ4  = q4Typedefs
typedefsFor WQ4K = q4kTypedefs

preambleFor :: Bool -> WeightQuant -> String
preambleFor False WQ8  = q8Preamble
preambleFor False WQ4  = q4Preamble
preambleFor False WQ4K = q4kPreamble
preambleFor True  WQ8  = q8VnniPreamble
preambleFor True  WQ4  = q4VnniPreamble
preambleFor True  WQ4K = q4kVnniPreamble

-- | Function parameters for the generated matvec.
matvecParams :: [(String, String)]
matvecParams =
  [ ("float* restrict", "out")
  , ("const float* restrict", "x")
  , ("const void* restrict", "W")
  , ("int64_t", "N")
  , ("int64_t", "KB")
  ]

-- ---------------------------------------------------------------------------
-- Compilation pipeline
-- ---------------------------------------------------------------------------

-- | Compile a matvec kernel for specific dimensions (Q8_0).
compileMatvec :: MatvecSchedule -> Int -> Int -> IO CompiledMatvec
compileMatvec = compileMatvecQ WQ8

-- | Compile a matvec kernel for specific dimensions with chosen quantization.
--
-- Pipeline:
-- 1. Select polyhedron based on schedule (tiled/untiled)
-- 2. Specialize with concrete N, KB
-- 3. Build scanner (instant with concrete params)
-- 4. Generate C with OpenMP/SIMD pragmas
-- 5. Compile with gcc -O3 -march=native -fopenmp
-- 6. Load via dlopen
--
-- The polyhedral domain is identical for Q8 and Q4 (both use 32-element blocks,
-- so KB = K/32 is the same). Only the innermost body differs.
--
-- When @schVnni sch@ is True, the kernel quantizes the activation vector to
-- Q8_0 in the preamble and uses int8×int8→int32 accumulation in the body,
-- enabling AVX-512 VNNI instructions.
compileMatvecQ :: WeightQuant -> MatvecSchedule -> Int -> Int -> IO CompiledMatvec
compileMatvecQ wq sch n kBlocks = do
  let nI  = fromIntegral n
      kbI = fromIntegral kBlocks
      vnni = schVnni sch
      prefix = case (wq, vnni) of
        (WQ8,  False) -> "q8mv_";      (WQ4,  False) -> "q4mv_"
        (WQ4K, False) -> "q4kmv_";     (WQ8,  True)  -> "q8mv_vnni_"
        (WQ4,  True)  -> "q4mv_vnni_"; (WQ4K, True)  -> "q4kmv_vnni_"
      funcName = prefix ++ schName sch

      -- Build the CKernel based on schedule config
      ck = buildCKernel wq sch funcName nI kbI

      src = generateC ck

  compiled <- compileAndLoad funcName src
  let fn = mkMatvecFn (castFunPtr (ckFuncPtr compiled))

  return CompiledMatvec
    { cmFn       = fn
    , cmSchedule = sch
    , cmN        = n
    , cmKBlocks  = kBlocks
    , cmKernel   = compiled
    , cmSource   = src
    }

-- | Build a CKernel from a schedule, weight quantization, and concrete dimensions.
buildCKernel :: WeightQuant -> MatvecSchedule -> String -> Integer -> Integer -> CKernel
buildCKernel wq sch funcName nI kbI =
  let vnni = schVnni sch
  in case (schTileJ sch, schTileK sch) of
    -- No tiling: flat loop
    (0, 0) ->
      let specialized = specialize [kbI, nI] matvecDomain
          Scanner nests = mkScanner (PDisjunction [specialized])
      in mkCK vnni wq funcName ["j", "kb"] nests
           Set.empty  -- no parallel (too small)
           -- No auto-SIMD: the body already has its own #pragma omp simd reduction
           (Set.fromList [99])

    -- Single-level J tiling: tj=0, j=1, kb=2
    (tj, 0) ->
      let specialized = specialize [kbI, nI] (tiledMatvecDomain (fromIntegral tj))
          Scanner nests = mkScanner (PDisjunction [specialized])
          parDims = if schParallel sch then Set.singleton 0 else Set.empty
      in mkCK vnni wq funcName ["tj", "j", "kb"] nests parDims
           (Set.fromList [99])  -- no auto-SIMD

    -- J + K tiling: tj=0, tkb=1, j=2, kb=3
    (tj, tk) ->
      let specialized = specialize [kbI, nI] (twoLevelMatvecDomain (fromIntegral tj) (fromIntegral tk))
          Scanner nests = mkScanner (PDisjunction [specialized])
          parDims = if schParallel sch then Set.singleton 0 else Set.empty
      in mkCK vnni wq funcName ["tj", "tkb", "j", "kb"] nests parDims
           (Set.fromList [99])  -- no auto-SIMD

-- | Assemble the CKernel record with weight-quant-appropriate body/typedefs.
mkCK :: Bool -> WeightQuant -> String -> [String] -> [LoopNest ps n] -> Set Int -> Set Int -> CKernel
mkCK vnni wq funcName dimNames nests parDims simdDims = CKernel
  { ckName         = funcName
  , ckIncludes     = []
  , ckTypedefs     = typedefsFor wq
  , ckMacros       = []
  , ckReturnType   = "void"
  , ckFuncParams   = matvecParams
  , ckDimNames     = dimNames
  , ckParamNames   = ["KB", "N"]
  , ckParallelDims = parDims
  , ckSimdDims     = simdDims
  , ckPreamble     = preambleFor vnni wq
  , ckBody         = bodyFor vnni wq
  , ckPostamble    = q8Postamble
  , ckLoopNests    = map eraseLoopNest nests
  }

-- ===========================================================================
-- Q8 GEMM (batched matrix-vector for prefill)
-- ===========================================================================

-- ---------------------------------------------------------------------------
-- GEMM schedule
-- ---------------------------------------------------------------------------

-- | Where to place the batch loop relative to the point loops.
--
-- The polyhedral scanner generates loops in dimension order (dim 0 = outermost).
-- Changing @b@'s position changes the entire reuse pattern without touching
-- the body or codegen. This is pure schedule — orthogonal to computation.
--
-- @
--   BOutermost:  tj → tkb → b → j → kb   (X locality: full x-slice per token)
--   BInnermost:  tj → tkb → j → kb → b   (W reuse: same block across B tokens)
--   BBetweenJK:  tj → tkb → j → b → kb   (X slice stays hot across 32-elem reduction)
-- @
data GemmBatchPos
  = BOutermost    -- ^ b before j and kb (default)
  | BInnermost    -- ^ b after j and kb (maximum weight reuse)
  | BBetweenJK    -- ^ b between j and kb (X-slice locality)
  deriving (Show, Eq, Enum, Bounded)

-- | Schedule for Q8_0 batched GEMM (prefill projections).
--
-- The base domain is @{ [b, j, kb] : 0 <= b < B, 0 <= j < N, 0 <= kb < KB }@.
-- Tiling wraps the j and kb dimensions for cache blocking and parallelism.
-- The batch position controls loop ordering within tiles.
data GemmSchedule = GemmSchedule
  { gsName     :: !String
  , gsTileJ    :: !Int         -- ^ J tile size. 0 = no J tiling.
  , gsTileKB   :: !Int         -- ^ KB tile size (in Q8 blocks). 0 = no KB tiling.
  , gsParallel :: !Bool        -- ^ OpenMP parallel on outermost tile loop.
  , gsBatchPos :: !GemmBatchPos -- ^ Batch loop placement.
  } deriving (Show)

-- | Default GEMM schedule: J-tiled for parallelism, no KB-tiling.
defaultGemmSchedule :: GemmSchedule
defaultGemmSchedule = GemmSchedule
  { gsName     = "gemm_default"
  , gsTileJ    = 128
  , gsTileKB   = 0
  , gsParallel = True
  , gsBatchPos = BOutermost
  }

-- ---------------------------------------------------------------------------
-- FFI type for the generated Q8 GEMM
-- ---------------------------------------------------------------------------

-- | C signature: void f(float* Y, float* X, void* W, int64_t B, int64_t N, int64_t KB)
type Q8GemmFnC = Ptr Float -> Ptr Float -> Ptr Word8
              -> Int64 -> Int64 -> Int64 -> IO ()

foreign import ccall "dynamic"
  mkQ8GemmFn :: FunPtr Q8GemmFnC -> Q8GemmFnC

-- | A compiled polyhedral Q8 GEMM kernel.
data CompiledQ8Gemm = CompiledQ8Gemm
  { cqFn       :: !Q8GemmFnC
  , cqSchedule :: !GemmSchedule
  , cqB        :: !Int              -- ^ B it was specialized for
  , cqN        :: !Int              -- ^ Output dimension
  , cqKBlocks  :: !Int              -- ^ KB = K / 32
  , cqKernel   :: !CompiledKernel
  , cqSource   :: !String
  }

-- | Call a compiled Q8 GEMM: Y[b,j] = W[j,:] @ X[b,:]
callQ8Gemm :: CompiledQ8Gemm -> Ptr Float -> Ptr Float -> Ptr Word8 -> Int -> IO ()
callQ8Gemm cq yPtr xPtr wPtr batchSize =
  cqFn cq yPtr xPtr wPtr
    (fromIntegral batchSize) (fromIntegral (cqN cq)) (fromIntegral (cqKBlocks cq))

-- ---------------------------------------------------------------------------
-- Parametric polyhedra for Q8 GEMM
-- ---------------------------------------------------------------------------

-- | Base Q8 GEMM domain (no tiling).
-- Ordering depends on 'GemmBatchPos':
--   BOutermost:  { [b, j, kb] }   — b=0, j=1, kb=2
--   BInnermost:  { [j, kb, b] }   — j=0, kb=1, b=2
--   BBetweenJK:  { [j, b, kb] }   — j=0, b=1, kb=2
q8GemmDomain :: GemmBatchPos -> PConjunction '["B", "KB", "N"] 3
q8GemmDomain BOutermost = mkPConjunction @'["B","KB","N"] @3 $
  \(bp :- kbp :- np :- Nil) (b :- j :- kb :- Nil) ->
        idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
    &&: idx j  >=: cst 0 &&: idx j  <=: idx np  -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1
q8GemmDomain BInnermost = mkPConjunction @'["B","KB","N"] @3 $
  \(bp :- kbp :- np :- Nil) (j :- kb :- b :- Nil) ->
        idx j  >=: cst 0 &&: idx j  <=: idx np  -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
q8GemmDomain BBetweenJK = mkPConjunction @'["B","KB","N"] @3 $
  \(bp :- kbp :- np :- Nil) (j :- b :- kb :- Nil) ->
        idx j  >=: cst 0 &&: idx j  <=: idx np  -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1

-- | J-tiled Q8 GEMM (4D). Batch position varies:
--   BOutermost:  { [tj, b, j, kb] }
--   BInnermost:  { [tj, j, kb, b] }
--   BBetweenJK:  { [tj, j, b, kb] }
tiledQ8GemmDomain :: GemmBatchPos -> Integer -> PConjunction '["B", "KB", "N"] 4
tiledQ8GemmDomain BOutermost t = mkPConjunction @'["B","KB","N"] @4 $
  \(bp :- kbp :- np :- Nil) (tj :- b :- j :- kb :- Nil) ->
        idx tj >=: cst 0 &&: t *: idx tj <=: idx np -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
    &&: idx j  >=: t *: idx tj &&: idx j <=: t *: idx tj +: cst (t - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1
tiledQ8GemmDomain BInnermost t = mkPConjunction @'["B","KB","N"] @4 $
  \(bp :- kbp :- np :- Nil) (tj :- j :- kb :- b :- Nil) ->
        idx tj >=: cst 0 &&: t *: idx tj <=: idx np -: cst 1
    &&: idx j  >=: t *: idx tj &&: idx j <=: t *: idx tj +: cst (t - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
tiledQ8GemmDomain BBetweenJK t = mkPConjunction @'["B","KB","N"] @4 $
  \(bp :- kbp :- np :- Nil) (tj :- j :- b :- kb :- Nil) ->
        idx tj >=: cst 0 &&: t *: idx tj <=: idx np -: cst 1
    &&: idx j  >=: t *: idx tj &&: idx j <=: t *: idx tj +: cst (t - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
    &&: idx kb >=: cst 0 &&: idx kb <=: idx kbp -: cst 1

-- | J + KB tiled Q8 GEMM (5D). Batch position varies:
--   BOutermost:  { [tj, tkb, b, j, kb] }
--   BInnermost:  { [tj, tkb, j, kb, b] }   — max weight reuse
--   BBetweenJK:  { [tj, tkb, j, b, kb] }   — X-slice locality
twoLevelQ8GemmDomain :: GemmBatchPos -> Integer -> Integer -> PConjunction '["B", "KB", "N"] 5
twoLevelQ8GemmDomain BOutermost tj tk = mkPConjunction @'["B","KB","N"] @5 $
  \(bp :- kbp :- np :- Nil) (tji :- tkbi :- b :- j :- kb :- Nil) ->
        idx tji  >=: cst 0 &&: tj *: idx tji <=: idx np  -: cst 1
    &&: idx tkbi >=: cst 0 &&: tk *: idx tkbi <=: idx kbp -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
    &&: idx j  >=: tj *: idx tji &&: idx j <=: tj *: idx tji +: cst (tj - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx kb >=: tk *: idx tkbi &&: idx kb <=: tk *: idx tkbi +: cst (tk - 1)
    &&: idx kb <=: idx kbp -: cst 1
twoLevelQ8GemmDomain BInnermost tj tk = mkPConjunction @'["B","KB","N"] @5 $
  \(bp :- kbp :- np :- Nil) (tji :- tkbi :- j :- kb :- b :- Nil) ->
        idx tji  >=: cst 0 &&: tj *: idx tji <=: idx np  -: cst 1
    &&: idx tkbi >=: cst 0 &&: tk *: idx tkbi <=: idx kbp -: cst 1
    &&: idx j  >=: tj *: idx tji &&: idx j <=: tj *: idx tji +: cst (tj - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx kb >=: tk *: idx tkbi &&: idx kb <=: tk *: idx tkbi +: cst (tk - 1)
    &&: idx kb <=: idx kbp -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
twoLevelQ8GemmDomain BBetweenJK tj tk = mkPConjunction @'["B","KB","N"] @5 $
  \(bp :- kbp :- np :- Nil) (tji :- tkbi :- j :- b :- kb :- Nil) ->
        idx tji  >=: cst 0 &&: tj *: idx tji <=: idx np  -: cst 1
    &&: idx tkbi >=: cst 0 &&: tk *: idx tkbi <=: idx kbp -: cst 1
    &&: idx j  >=: tj *: idx tji &&: idx j <=: tj *: idx tji +: cst (tj - 1)
    &&: idx j  <=: idx np -: cst 1
    &&: idx b  >=: cst 0 &&: idx b  <=: idx bp  -: cst 1
    &&: idx kb >=: tk *: idx tkbi &&: idx kb <=: tk *: idx tkbi +: cst (tk - 1)
    &&: idx kb <=: idx kbp -: cst 1

-- ---------------------------------------------------------------------------
-- Q8 GEMM body template
-- ---------------------------------------------------------------------------

-- | Innermost loop body for Q8 GEMM.
-- Variables available: b (batch index), j (output row), kb (K block index).
-- Arrays: Y (float*), X (float*), W (block_q8_0*).
-- Macros: KB (number of K blocks).
q8GemmBody :: String
q8GemmBody = unlines
  [ "{"
  , "    const block_q8_0* blk = &W_q8[j * KB + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    float block_sum = 0.0f;"
  , "    #pragma omp simd reduction(+:block_sum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        block_sum += X[b * (KB * 32) + kb * 32 + v] * (float)blk->qs[v];"
  , "    }"
  , "    Y[b * N + j] += scale * block_sum;"
  , "}"
  ]

q8GemmPreamble :: Int -> String
q8GemmPreamble _bMax = unlines
  [ "    const block_q8_0* W_q8 = (const block_q8_0*)W;"
  , "    for (int64_t i = 0; i < (int64_t)B * N; i++) Y[i] = 0.0f;"
  ]

-- | Innermost loop body for Q4 GEMM.
q4GemmBody :: String
q4GemmBody = unlines
  [ "{"
  , "    const block_q4_0* blk = &W_q4[j * KB + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    float block_sum = 0.0f;"
  , "    #pragma omp simd reduction(+:block_sum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        int nibble = (blk->qs[v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "        block_sum += X[b * (KB * 32) + kb * 32 + v] * (float)(nibble - 8);"
  , "    }"
  , "    Y[b * N + j] += scale * block_sum;"
  , "}"
  ]

q4GemmPreamble :: Int -> String
q4GemmPreamble _bMax = unlines
  [ "    const block_q4_0* W_q4 = (const block_q4_0*)W;"
  , "    for (int64_t i = 0; i < (int64_t)B * N; i++) Y[i] = 0.0f;"
  ]

-- ---------------------------------------------------------------------------
-- VNNI GEMM body templates
-- ---------------------------------------------------------------------------

-- | Q8 GEMM body with VNNI int8×int8→int32 accumulation.
-- Reads from Xq8/Xd arrays quantized in the preamble.
q8VnniGemmBody :: String
q8VnniGemmBody = unlines
  [ "{"
  , "    const block_q8_0* blk = &W_q8[j * KB + kb];"
  , "    int32_t isum = 0;"
  , "    #pragma omp simd reduction(+:isum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        isum += (int32_t)blk->qs[v] * (int32_t)Xq8[b * (KB * 32) + kb * 32 + v];"
  , "    }"
  , "    Y[b * N + j] += f16_to_f32(blk->d) * Xd[b * KB + kb] * (float)isum;"
  , "}"
  ]

-- | VNNI GEMM preamble: quantize all B activation vectors to Q8_0.
-- Uses malloc since B*KB*32 can be ~512KB for large batches.
q8VnniGemmPreamble :: Int -> String
q8VnniGemmPreamble _bMax = unlines
  [ "    const block_q8_0* W_q8 = (const block_q8_0*)W;"
  , "    for (int64_t i = 0; i < (int64_t)B * N; i++) Y[i] = 0.0f;"
  , "    /* Quantize all B activation vectors to Q8_0 for VNNI */"
  , "    int8_t* Xq8 = (int8_t*)malloc((size_t)B * KB * 32);"
  , "    float* Xd = (float*)malloc((size_t)B * KB * sizeof(float));"
  , "    for (int64_t bi = 0; bi < B; bi++) {"
  , "        for (int64_t qb = 0; qb < KB; qb++) {"
  , "            float amax = 0.0f;"
  , "            for (int v = 0; v < 32; v++) {"
  , "                float av = fabsf(X[bi * KB * 32 + qb * 32 + v]);"
  , "                if (av > amax) amax = av;"
  , "            }"
  , "            Xd[bi * KB + qb] = amax / 127.0f;"
  , "            float id = amax > 0.0f ? 127.0f / amax : 0.0f;"
  , "            for (int v = 0; v < 32; v++)"
  , "                Xq8[bi * KB * 32 + qb * 32 + v] = (int8_t)roundf(X[bi * KB * 32 + qb * 32 + v] * id);"
  , "        }"
  , "    }"
  ]

q8VnniGemmPostamble :: String
q8VnniGemmPostamble = unlines
  [ "    free(Xq8);"
  , "    free(Xd);"
  ]

-- | Q4 GEMM body with VNNI int8×int8→int32 accumulation.
q4VnniGemmBody :: String
q4VnniGemmBody = unlines
  [ "{"
  , "    const block_q4_0* blk = &W_q4[j * KB + kb];"
  , "    int32_t isum = 0;"
  , "    #pragma omp simd reduction(+:isum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        int nibble = (blk->qs[v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "        isum += (int32_t)(int8_t)(nibble - 8) * (int32_t)Xq8[b * (KB * 32) + kb * 32 + v];"
  , "    }"
  , "    Y[b * N + j] += f16_to_f32(blk->d) * Xd[b * KB + kb] * (float)isum;"
  , "}"
  ]

q4VnniGemmPreamble :: Int -> String
q4VnniGemmPreamble _bMax = unlines
  [ "    const block_q4_0* W_q4 = (const block_q4_0*)W;"
  , "    for (int64_t i = 0; i < (int64_t)B * N; i++) Y[i] = 0.0f;"
  , "    /* Quantize all B activation vectors to Q8_0 for VNNI */"
  , "    int8_t* Xq8 = (int8_t*)malloc((size_t)B * KB * 32);"
  , "    float* Xd = (float*)malloc((size_t)B * KB * sizeof(float));"
  , "    for (int64_t bi = 0; bi < B; bi++) {"
  , "        for (int64_t qb = 0; qb < KB; qb++) {"
  , "            float amax = 0.0f;"
  , "            for (int v = 0; v < 32; v++) {"
  , "                float av = fabsf(X[bi * KB * 32 + qb * 32 + v]);"
  , "                if (av > amax) amax = av;"
  , "            }"
  , "            Xd[bi * KB + qb] = amax / 127.0f;"
  , "            float id = amax > 0.0f ? 127.0f / amax : 0.0f;"
  , "            for (int v = 0; v < 32; v++)"
  , "                Xq8[bi * KB * 32 + qb * 32 + v] = (int8_t)roundf(X[bi * KB * 32 + qb * 32 + v] * id);"
  , "        }"
  , "    }"
  ]

-- | Q4_K GEMM body (float path).
q4kGemmBody :: String
q4kGemmBody = unlines
  [ "{"
  , "    int64_t sb = kb / 8;"
  , "    int64_t si = kb & 7;"
  , "    const block_q4_K* sblk = &W_q4k[j * (KB / 8) + sb];"
  , "    uint8_t utmp[2];"
  , "    if (si < 4) {"
  , "        utmp[0] = sblk->scales[si] & 63;"
  , "        utmp[1] = sblk->scales[si + 4] & 63;"
  , "    } else {"
  , "        utmp[0] = (sblk->scales[si + 4] & 0xF) | ((sblk->scales[si - 4] >> 6) << 4);"
  , "        utmp[1] = (sblk->scales[si + 4] >>  4) | ((sblk->scales[si]     >> 6) << 4);"
  , "    }"
  , "    float d_sc = f16_to_f32(sblk->d) * utmp[0];"
  , "    float d_mn = f16_to_f32(sblk->dmin) * utmp[1];"
  , "    int64_t qoff = si * 16;"
  , "    float block_sum = 0.0f;"
  , "    #pragma omp simd reduction(+:block_sum)"
  , "    for (int v = 0; v < 32; v++) {"
  , "        int nibble = (sblk->qs[qoff + v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "        block_sum += X[b * (KB * 32) + kb * 32 + v] * (d_sc * (float)nibble - d_mn);"
  , "    }"
  , "    Y[b * N + j] += block_sum;"
  , "}"
  ]

q4kGemmPreamble :: Int -> String
q4kGemmPreamble _bMax = unlines
  [ "    const block_q4_K* W_q4k = (const block_q4_K*)W;"
  , "    for (int64_t i = 0; i < (int64_t)B * N; i++) Y[i] = 0.0f;"
  ]

-- | Select GEMM body/preamble/postamble based on weight quantization and VNNI mode.
gemmBodyFor :: Bool -> WeightQuant -> String
gemmBodyFor False WQ8  = q8GemmBody
gemmBodyFor False WQ4  = q4GemmBody
gemmBodyFor False WQ4K = q4kGemmBody
gemmBodyFor True  WQ8  = q8VnniGemmBody
gemmBodyFor True  WQ4  = q4VnniGemmBody
gemmBodyFor True  WQ4K = q4kGemmBody  -- Q4_K VNNI GEMM not yet implemented, use float

gemmPreambleFor :: Bool -> WeightQuant -> Int -> String
gemmPreambleFor False WQ8  = q8GemmPreamble
gemmPreambleFor False WQ4  = q4GemmPreamble
gemmPreambleFor False WQ4K = q4kGemmPreamble
gemmPreambleFor True  WQ8  = q8VnniGemmPreamble
gemmPreambleFor True  WQ4  = q4VnniGemmPreamble
gemmPreambleFor True  WQ4K = q4kGemmPreamble  -- fallback to float

gemmPostambleFor :: Bool -> WeightQuant -> String
gemmPostambleFor True WQ8 = q8VnniGemmPostamble
gemmPostambleFor True WQ4 = q8VnniGemmPostamble  -- same free() logic
gemmPostambleFor _    _   = ""

q8GemmParams :: [(String, String)]
q8GemmParams =
  [ ("float* restrict", "Y")
  , ("const float* restrict", "X")
  , ("const void* restrict", "W")
  , ("int64_t", "B")
  , ("int64_t", "N")
  , ("int64_t", "KB")
  ]

-- ---------------------------------------------------------------------------
-- GEMM compilation pipeline
-- ---------------------------------------------------------------------------

-- | Compile a Q8 GEMM kernel for specific dimensions.
compileQ8Gemm :: GemmSchedule -> Int -> Int -> Int -> IO CompiledQ8Gemm
compileQ8Gemm = compileGemmQ WQ8

-- | Compile a GEMM kernel with chosen weight quantization.
--
-- Pipeline (same as 'compileMatvec' but with 3D+ GEMM domain):
-- 1. Select polyhedron based on schedule (tiled/untiled)
-- 2. Specialize with concrete B, N, KB
-- 3. Build scanner (instant with concrete params)
-- 4. Generate C with OpenMP/SIMD pragmas
-- 5. Compile with gcc -O3 -march=native -fopenmp
-- 6. Load via dlopen
--
-- The @vnni@ flag is threaded from the matvec schedule's 'schVnni'.
compileGemmQ :: WeightQuant -> GemmSchedule -> Int -> Int -> Int -> IO CompiledQ8Gemm
compileGemmQ = compileGemmQV False

-- | Like 'compileGemmQ' but with explicit VNNI flag.
compileGemmQV :: Bool -> WeightQuant -> GemmSchedule -> Int -> Int -> Int -> IO CompiledQ8Gemm
compileGemmQV vnni wq sch batchMax n kBlocks = do
  let bI  = fromIntegral batchMax
      nI  = fromIntegral n
      kbI = fromIntegral kBlocks
      prefix = case (wq, vnni) of
        (WQ8,  False) -> "q8gemm_";       (WQ4,  False) -> "q4gemm_"
        (WQ4K, False) -> "q4kgemm_";      (WQ8,  True)  -> "q8gemm_vnni_"
        (WQ4,  True)  -> "q4gemm_vnni_";  (WQ4K, True)  -> "q4kgemm_vnni_"
      funcName = prefix ++ gsName sch

      ck = buildGemmCKernel vnni wq sch funcName bI nI kbI
      src = generateC ck

  compiled <- compileAndLoad funcName src
  let fn = mkQ8GemmFn (castFunPtr (ckFuncPtr compiled))

  return CompiledQ8Gemm
    { cqFn       = fn
    , cqSchedule = sch
    , cqB        = batchMax
    , cqN        = n
    , cqKBlocks  = kBlocks
    , cqKernel   = compiled
    , cqSource   = src
    }

-- | Build a CKernel from a GEMM schedule, weight quant, and concrete dimensions.
-- B is kept as a runtime parameter (not specialized) so the loop respects
-- the actual batch size.  KB and N are specialized to concrete values.
buildGemmCKernel :: Bool -> WeightQuant -> GemmSchedule -> String -> Integer -> Integer -> Integer -> CKernel
buildGemmCKernel vnni wq sch funcName _bI nI kbI =
  let bp = gsBatchPos sch
      -- Params are ["B", "KB", "N"]; keep B symbolic, specialize KB and N.
      partialParams = [Nothing, Just kbI, Just nI]
  in case (gsTileJ sch, gsTileKB sch) of
    -- No tiling: flat loops (3D)
    (0, 0) ->
      let specialized = specializePartial partialParams (q8GemmDomain bp)
          Scanner nests = mkScanner (PDisjunction [specialized])
      in mkGemmCK vnni wq funcName (untiled3Names bp) nests
           Set.empty (Set.fromList [99])

    -- J-tiled (4D)
    (tj, 0) ->
      let specialized = specializePartial partialParams (tiledQ8GemmDomain bp (fromIntegral tj))
          Scanner nests = mkScanner (PDisjunction [specialized])
          parDims = if gsParallel sch then Set.singleton 0 else Set.empty
      in mkGemmCK vnni wq funcName (tiled4Names bp) nests
           parDims (Set.fromList [99])

    -- J + KB tiled (5D)
    (tj, tk) ->
      let specialized = specializePartial partialParams
                          (twoLevelQ8GemmDomain bp (fromIntegral tj) (fromIntegral tk))
          Scanner nests = mkScanner (PDisjunction [specialized])
          parDims = if gsParallel sch then Set.singleton 0 else Set.empty
      in mkGemmCK vnni wq funcName (tiled5Names bp) nests
           parDims (Set.fromList [99])

-- | Dimension names for each batch position (must match domain dim order).
untiled3Names :: GemmBatchPos -> [String]
untiled3Names BOutermost  = ["b", "j", "kb"]
untiled3Names BInnermost  = ["j", "kb", "b"]
untiled3Names BBetweenJK  = ["j", "b", "kb"]

tiled4Names :: GemmBatchPos -> [String]
tiled4Names BOutermost  = ["tj", "b", "j", "kb"]
tiled4Names BInnermost  = ["tj", "j", "kb", "b"]
tiled4Names BBetweenJK  = ["tj", "j", "b", "kb"]

tiled5Names :: GemmBatchPos -> [String]
tiled5Names BOutermost  = ["tj", "tkb", "b", "j", "kb"]
tiled5Names BInnermost  = ["tj", "tkb", "j", "kb", "b"]
tiled5Names BBetweenJK  = ["tj", "tkb", "j", "b", "kb"]

-- | Assemble the CKernel record for GEMM with weight-quant-appropriate bodies.
mkGemmCK :: Bool -> WeightQuant -> String -> [String] -> [LoopNest ps n] -> Set Int -> Set Int -> CKernel
mkGemmCK vnni wq funcName dimNames nests parDims simdDims = CKernel
  { ckName         = funcName
  , ckIncludes     = if vnni then ["<stdlib.h>"] else []
  , ckTypedefs     = typedefsFor wq
  , ckMacros       = []
  , ckReturnType   = "void"
  , ckFuncParams   = q8GemmParams
  , ckDimNames     = dimNames
  , ckParamNames   = ["B"]  -- B is the only remaining runtime parameter
  , ckParallelDims = parDims
  , ckSimdDims     = simdDims
  , ckPreamble     = gemmPreambleFor vnni wq 0
  , ckBody         = gemmBodyFor vnni wq
  , ckPostamble    = gemmPostambleFor vnni wq
  , ckLoopNests    = map eraseLoopNest nests
  }

