{-# LANGUAGE ForeignFunctionInterface #-}

-- | Polyhedral Q8_0 matrix-vector kernel.
--
-- Defines the matvec iteration domain as a parametric polyhedron,
-- applies schedule transforms, verifies at generation time via ISL,
-- specializes with concrete dimensions, scans, generates C, compiles.
--
-- The generated kernel replaces the hand-written @q8_matvec@ from cbits.
module Isl.Infer.Kernel.GEMM
  ( -- * Kernel compilation
    CompiledMatvec(..)
  , compileMatvec
    -- * Polyhedron definitions (exported for inspection/testing)
  , matvecDomain
  , tiledMatvecDomain
  , twoLevelMatvecDomain
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
  ]

q8Preamble :: String
q8Preamble = unlines
  [ "    const block_q8_0* W_q8 = (const block_q8_0*)W;"
  , "    for (int64_t jj = 0; jj < N; jj++) out[jj] = 0.0f;"
  ]

q8Postamble :: String
q8Postamble = ""

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

-- | Compile a matvec kernel for specific dimensions.
--
-- Pipeline:
-- 1. Select polyhedron based on schedule (tiled/untiled)
-- 2. Specialize with concrete N, KB
-- 3. Build scanner (instant with concrete params)
-- 4. Generate C with OpenMP/SIMD pragmas
-- 5. Compile with gcc -O3 -march=native -fopenmp
-- 6. Load via dlopen
--
-- The schedule is checked for consistency (tile sizes must divide evenly
-- or the scanner handles remainders via min-bounds).
compileMatvec :: MatvecSchedule -> Int -> Int -> IO CompiledMatvec
compileMatvec sch n kBlocks = do
  let nI  = fromIntegral n
      kbI = fromIntegral kBlocks
      funcName = "q8mv_" ++ schName sch

      -- Build the CKernel based on schedule config
      ck = buildCKernel sch funcName nI kbI

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

-- | Build a CKernel from a schedule and concrete dimensions.
buildCKernel :: MatvecSchedule -> String -> Integer -> Integer -> CKernel
buildCKernel sch funcName nI kbI =
  case (schTileJ sch, schTileK sch) of
    -- No tiling: flat loop
    (0, 0) ->
      let specialized = specialize [kbI, nI] matvecDomain
          Scanner nests = mkScanner (PDisjunction [specialized])
      in mkCK funcName ["j", "kb"] nests
           Set.empty  -- no parallel (too small)
           -- No auto-SIMD: the body already has its own #pragma omp simd reduction
           (Set.fromList [99])

    -- Single-level J tiling: tj=0, j=1, kb=2
    (tj, 0) ->
      let specialized = specialize [kbI, nI] (tiledMatvecDomain (fromIntegral tj))
          Scanner nests = mkScanner (PDisjunction [specialized])
          parDims = if schParallel sch then Set.singleton 0 else Set.empty
      in mkCK funcName ["tj", "j", "kb"] nests parDims
           (Set.fromList [99])  -- no auto-SIMD

    -- J + K tiling: tj=0, tkb=1, j=2, kb=3
    (tj, tk) ->
      let specialized = specialize [kbI, nI] (twoLevelMatvecDomain (fromIntegral tj) (fromIntegral tk))
          Scanner nests = mkScanner (PDisjunction [specialized])
          parDims = if schParallel sch then Set.singleton 0 else Set.empty
      in mkCK funcName ["tj", "tkb", "j", "kb"] nests parDims
           (Set.fromList [99])  -- no auto-SIMD

-- | Assemble the CKernel record.
mkCK :: String -> [String] -> [LoopNest ps n] -> Set Int -> Set Int -> CKernel
mkCK funcName dimNames nests parDims simdDims = CKernel
  { ckName         = funcName
  , ckIncludes     = []
  , ckTypedefs     = q8Typedefs
  , ckMacros       = []
  , ckReturnType   = "void"
  , ckFuncParams   = matvecParams
  , ckDimNames     = dimNames
  , ckParamNames   = ["KB", "N"]
  , ckParallelDims = parDims
  , ckSimdDims     = simdDims
  , ckPreamble     = q8Preamble
  , ckBody         = q8Body
  , ckPostamble    = q8Postamble
  , ckLoopNests    = map eraseLoopNest nests
  }

