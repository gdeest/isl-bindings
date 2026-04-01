{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Polyhedral specification of panel-packed matvec\/GEMM kernels.
--
-- Replaces the hand-crafted C string builders in "Isl.Infer.Kernel.Packed"
-- with polyhedral domain definitions fed through the multi-statement
-- scanner and C codegen pipeline.
--
-- The loop nests are ISL-generated; only the innermost statement bodies
-- (dequant, broadcast-FMA) remain hand-written C.
module Isl.Infer.Kernel.PackedPoly
  ( -- * Multi-statement kernel specifications
    packedMatvecSpec
  , packedMatvecSpecParametric
  , packedGemmSpec
    -- * Compilation (drop-in replacements)
  , CompiledPackedMatvecPoly(..)
  , compilePackedMatvecPoly
    -- * Polyhedral domain definitions (for reuse in fused layer)
  , s1Domain
  , s2Domain
  , s1Body
  , s2Body
  , mkPackedMultiScannerParametric
    -- * Shared typedefs
  , packedTypedefs
  ) where

import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction(..), mkPConjunction)
import Isl.Scan.Build (buildLevels)
import Isl.Scan.Multi
  ( MultiScanner(..), ScheduledStatement(..), ULoopNest(..), StmtInverse(..) )
import Isl.Infer.Specialize (specialize)

import Isl.Infer.Arch
import Isl.Infer.Kernel.GEMM (WeightQuant(..))
import Isl.Infer.Codegen.Multi
import Isl.Infer.Runtime

-- ---------------------------------------------------------------------------
-- FFI type (same as Packed.hs for drop-in replacement)
-- ---------------------------------------------------------------------------

type MatvecFnC = Ptr Float -> Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> IO ()

foreign import ccall "dynamic"
  mkMatvecFn :: FunPtr MatvecFnC -> MatvecFnC

-- ---------------------------------------------------------------------------
-- Polyhedral domain definitions
-- ---------------------------------------------------------------------------

-- | S1 (dequant) scheduled domain in time space (parametric in N, KB):
--
-- @[KB, N] -> { [tj, tkb, seq, dj, kb, v] :
--    0 <= tj, TJ*tj <= N-1,
--    0 <= tkb, TKB*tkb <= KB-1,
--    seq = 0,
--    0 <= dj <= TJ-1, TJ*tj + dj <= N-1,
--    TKB*tkb <= kb <= min(TKB*tkb + TKB-1, KB-1),
--    0 <= v <= 31 }@
--
-- Tile sizes (TJ, TKB) are Haskell-level constants baked into coefficients.
-- N and KB remain as ISL parameters, specialized at compile time.
s1Domain :: Integer -> Integer
         -> PConjunction '["KB", "N"] 6
s1Domain tjSz tkbSz = mkPConjunction @'["KB","N"] @6 $
  \(kbp :- np :- Nil) (d_tj :- d_tkb :- d_seq :- d_dj :- d_kb :- d_v :- Nil) ->
    -- tj: 0 <= tj, TJ*tj <= N-1
        idx d_tj >=: cst 0
    &&: tjSz *: idx d_tj <=: idx np -: cst 1
    -- tkb: 0 <= tkb, TKB*tkb <= KB-1
    &&: idx d_tkb >=: cst 0
    &&: tkbSz *: idx d_tkb <=: idx kbp -: cst 1
    -- seq = 0 (S1 before S2)
    &&: idx d_seq ==: cst 0
    -- dj: 0 <= dj <= TJ-1, TJ*tj + dj <= N-1
    &&: idx d_dj >=: cst 0
    &&: idx d_dj <=: cst (tjSz - 1)
    &&: tjSz *: idx d_tj +: idx d_dj <=: idx np -: cst 1
    -- kb: TKB*tkb <= kb <= min(TKB*tkb + TKB-1, KB-1)
    &&: idx d_kb >=: tkbSz *: idx d_tkb
    &&: idx d_kb <=: tkbSz *: idx d_tkb +: cst (tkbSz - 1)
    &&: idx d_kb <=: idx kbp -: cst 1
    -- v: 0 <= v <= 31
    &&: idx d_v >=: cst 0
    &&: idx d_v <=: cst 31

-- | S2 (broadcast-FMA) scheduled domain in time space (parametric in N, KB):
--
-- @[KB, N] -> { [tj, tkb, seq, k, dj, pad] : ..., seq = 1, pad = 0 }@
--
-- k iterates over scalar elements within the k-tile.
s2Domain :: Integer -> Integer
         -> PConjunction '["KB", "N"] 6
s2Domain tjSz tkbSz = mkPConjunction @'["KB","N"] @6 $
  \(kbp :- np :- Nil) (d_tj :- d_tkb :- d_seq :- d_k :- d_dj :- d_pad :- Nil) ->
    -- tj: 0 <= tj, TJ*tj <= N-1
        idx d_tj >=: cst 0
    &&: tjSz *: idx d_tj <=: idx np -: cst 1
    -- tkb: 0 <= tkb, TKB*tkb <= KB-1
    &&: idx d_tkb >=: cst 0
    &&: tkbSz *: idx d_tkb <=: idx kbp -: cst 1
    -- seq = 1 (S2 after S1)
    &&: idx d_seq ==: cst 1
    -- k: 0 <= k <= TKB*32-1, k + 32*TKB*tkb <= KB*32 - 1
    &&: idx d_k >=: cst 0
    &&: idx d_k <=: cst (tkbSz * 32 - 1)
    &&: idx d_k +: (32 * tkbSz) *: idx d_tkb <=: 32 *: idx kbp -: cst 1
    -- dj: 0 <= dj <= TJ-1, TJ*tj + dj <= N-1
    &&: idx d_dj >=: cst 0
    &&: idx d_dj <=: cst (tjSz - 1)
    &&: tjSz *: idx d_tj +: idx d_dj <=: idx np -: cst 1
    -- pad = 0
    &&: idx d_pad ==: cst 0

-- ---------------------------------------------------------------------------
-- Multi-statement scanner construction
-- ---------------------------------------------------------------------------

-- | Build a specialized MultiScanner for the packed matvec two-statement program.
--
-- The parametric domains ('s1Domain', 's2Domain') are specialized with
-- concrete @N@ and @KB@ values. Tile sizes are already baked into the
-- domain constraints as integer coefficients.
mkPackedMultiScanner :: Integer -> Integer -> Integer -> Integer -> MultiScanner 0
mkPackedMultiScanner n kb tjSize tkbSize =
  let -- Specialize parameters [KB, N] → concrete values
      -- (alphabetical order: KB before N)
      params = [kb, n]

      PConjunction s1Conj = specialize params (s1Domain tjSize tkbSize)
      (s1Levels, s1Params, s1Dims) = buildLevels (let Conjunction cs = s1Conj in cs)
      s1Nest = ULoopNest s1Levels s1Params s1Dims

      PConjunction s2Conj = specialize params (s2Domain tjSize tkbSize)
      (s2Levels, s2Params, s2Dims) = buildLevels (let Conjunction cs = s2Conj in cs)
      s2Nest = ULoopNest s2Levels s2Params s2Dims

      -- Trivial inverses (time coords = original coords for our purposes)
      trivialInverse nDims = StmtInverse nDims []

  in MultiScanner
    [ ScheduledStatement "S1" [s1Nest] (trivialInverse 6)
    , ScheduledStatement "S2" [s2Nest] (trivialInverse 6)
    ]

-- | Build a parametric MultiScanner — N and KB remain as runtime parameters.
--
-- The generated C uses @N@ and @KB@ as function-parameter variables in
-- loop bounds, producing a single compiled kernel that works for any
-- matrix dimensions (within the tile structure).
mkPackedMultiScannerParametric :: Integer -> Integer -> MultiScanner 2
mkPackedMultiScannerParametric tjSize tkbSize =
  let PConjunction s1Conj = s1Domain tjSize tkbSize
      (s1Levels, s1Params, s1Dims) = buildLevels (let Conjunction cs = s1Conj in cs)
      s1Nest = ULoopNest s1Levels s1Params s1Dims

      PConjunction s2Conj = s2Domain tjSize tkbSize
      (s2Levels, s2Params, s2Dims) = buildLevels (let Conjunction cs = s2Conj in cs)
      s2Nest = ULoopNest s2Levels s2Params s2Dims

      trivialInverse nDims = StmtInverse nDims []

  in MultiScanner
    [ ScheduledStatement "S1" [s1Nest] (trivialInverse 6)
    , ScheduledStatement "S2" [s2Nest] (trivialInverse 6)
    ]

-- ---------------------------------------------------------------------------
-- Statement bodies
-- ---------------------------------------------------------------------------

-- | S1 dequant body for Q8_0.
-- Expects loop vars: tj, tkb, seq (=0), dj, kb, v
s1BodyQ8 :: String
s1BodyQ8 = unlines
  [ "{"
  , "    int64_t j = tj * TJ + dj;"
  , "    const block_q8_0* blk = &W_q8[j * KB + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    int64_t base = (kb - tkb * TKB) * 32;"
  , "    panel[(base + v) * TJ + dj] = scale * (float)blk->qs[v];"
  , "}"
  ]

-- | S1 dequant body for Q4_0.
s1BodyQ4 :: String
s1BodyQ4 = unlines
  [ "{"
  , "    int64_t j = tj * TJ + dj;"
  , "    const block_q4_0* blk = &W_q4[j * KB + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    int64_t base = (kb - tkb * TKB) * 32;"
  , "    int nibble = (blk->qs[v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "    panel[(base + v) * TJ + dj] = scale * (float)(nibble - 8);"
  , "}"
  ]

-- | S1 dequant body for Q4_K.
s1BodyQ4K :: String
s1BodyQ4K = unlines
  [ "{"
  , "    int64_t j = tj * TJ + dj;"
  , "    int64_t sb = kb / 8; int64_t si = kb & 7;"
  , "    const block_q4_K* sblk = &W_q4k[j * (KB / 8) + sb];"
  , "    uint8_t utmp0, utmp1;"
  , "    if (si < 4) {"
  , "        utmp0 = sblk->scales[si] & 63;"
  , "        utmp1 = sblk->scales[si + 4] & 63;"
  , "    } else {"
  , "        utmp0 = (sblk->scales[si+4]&0xF)|((sblk->scales[si-4]>>6)<<4);"
  , "        utmp1 = (sblk->scales[si+4]>>4)|((sblk->scales[si]>>6)<<4);"
  , "    }"
  , "    float d_sc = f16_to_f32(sblk->d) * utmp0;"
  , "    float d_mn = f16_to_f32(sblk->dmin) * utmp1;"
  , "    int64_t qoff = si * 16;"
  , "    int64_t base = (kb - tkb * TKB) * 32;"
  , "    int nibble = (sblk->qs[qoff + v/2] >> ((v&1)*4)) & 0xF;"
  , "    panel[(base + v) * TJ + dj] = d_sc * (float)nibble - d_mn;"
  , "}"
  ]

-- | S2 broadcast-FMA body (quant-independent — works on float panel).
-- Expects loop vars: tj, tkb, seq (=1), k, dj
s2Body :: String
s2Body = unlines
  [ "{"
  , "    float xk = x[(tkb * TKB * 32) + k];"
  , "    acc[dj] += xk * panel[k * TJ + dj];"
  , "}"
  ]

-- | Select S1 body by weight quantization.
s1Body :: WeightQuant -> String
s1Body WQ8  = s1BodyQ8
s1Body WQ4  = s1BodyQ4
s1Body WQ4K = s1BodyQ4K

-- ---------------------------------------------------------------------------
-- MultiCKernel assembly
-- ---------------------------------------------------------------------------

-- | Build a 'MultiCKernel' for the panel-packed matvec.
packedMatvecSpec :: WeightQuant -> Arch -> Tiles -> Int -> Int -> MultiCKernel 0
packedMatvecSpec wq arch tiles n kBlocks = MultiCKernel
  { mckName         = funcName
  , mckIncludes     = []
  , mckTypedefs     = unlines (packedTypedefs wq)
  , mckMacros       = [ ("TJ",  show tj)
                       , ("TKB", show tkb)
                       , ("TK",  show tk)
                       , ("SIMD_W", show (archSimdWidth arch))
                       ]
  , mckReturnType   = "void"
  , mckFuncParams   = [ ("float* restrict", "out")
                       , ("const float* restrict", "x")
                       , ("const void* restrict", "W")
                       , ("int64_t", "N")
                       , ("int64_t", "KB")
                       ]
  , mckTimeDimNames = ["tj", "tkb", "seq", "d3", "d4", "d5"]
    -- d3/d4/d5 differ per statement (dj/kb/v for S1, k/dj/pad for S2)
    -- but the MergedAST handles divergent inner loops correctly
  , mckParamNames   = []
  , mckParallelDims = Set.singleton 0  -- tj is parallel
  , mckSimdDims     = Set.empty        -- we handle SIMD in bodies
  , mckPreamble     = unlines $
      packedWCast wq ++
      [ "    const int64_t K = KB * 32;" ]
  , mckPostamble    = ""
  , mckStmtBodies   =
      [ CStmtBody "S1" (s1Body wq) PlainStmt
          [("d3", "dj"), ("d4", "kb"), ("d5", "v")]
      , CStmtBody "S2" s2Body
          (ReductionStmt
            { rsInitCode     = accInit
            , rsFinalizeCode = accStore
            , rsReductionDim = "tkb"
            })
          [("d3", "k"), ("d4", "dj")]
      ]
  , mckScratch      =
      [ ScratchDecl "acc"   "float" "TJ" 64 "tj"
      , ScratchDecl "panel" "float" "TK * TJ" 64 "tkb"
      ]
  , mckScanner      = mkPackedMultiScanner
                         (fromIntegral n) (fromIntegral kBlocks)
                         (fromIntegral tj) (fromIntegral tkb)
  }
  where
    prefix = case wq of WQ8 -> "q8mv_packed_"; WQ4 -> "q4mv_packed_"; WQ4K -> "q4kmv_packed_"
    funcName = prefix ++ archName arch ++ "_poly"
    tj  = tileJ tiles
    tk  = tileK tiles
    tkb = tk `div` 32
    accInit = unlines
      [ "int64_t nj = TJ;"
      , "if (tj * TJ + TJ > N) nj = N - tj * TJ;"
      , "memset(acc, 0, nj * sizeof(float));"
      ]
    accStore = unlines
      [ "{ int64_t nj_ = TJ;"
      , "if (tj * TJ + TJ > N) nj_ = N - tj * TJ;"
      , "memcpy(&out[tj * TJ], acc, nj_ * sizeof(float)); }"
      ]

-- | Parametric variant: N and KB are runtime C variables in the loop bounds.
-- A single compiled kernel works for any matrix dimensions.
packedMatvecSpecParametric :: WeightQuant -> Arch -> Tiles -> MultiCKernel 2
packedMatvecSpecParametric wq arch tiles = MultiCKernel
  { mckName         = funcName
  , mckIncludes     = []
  , mckTypedefs     = unlines (packedTypedefs wq)
  , mckMacros       = [ ("TJ",  show tj)
                       , ("TKB", show tkb)
                       , ("TK",  show tk)
                       , ("SIMD_W", show (archSimdWidth arch))
                       ]
  , mckReturnType   = "void"
  , mckFuncParams   = [ ("float* restrict", "out")
                       , ("const float* restrict", "x")
                       , ("const void* restrict", "W")
                       , ("int64_t", "N")
                       , ("int64_t", "KB")
                       ]
  , mckTimeDimNames = ["tj", "tkb", "seq", "d3", "d4", "d5"]
  , mckParamNames   = ["KB", "N"]  -- ISL parameter names (alphabetical)
  , mckParallelDims = Set.singleton 0
  , mckSimdDims     = Set.empty
  , mckPreamble     = unlines $
      packedWCast wq ++
      [ "    const int64_t K = KB * 32;" ]
  , mckPostamble    = ""
  , mckStmtBodies   =
      [ CStmtBody "S1" (s1Body wq) PlainStmt
          [("d3", "dj"), ("d4", "kb"), ("d5", "v")]
      , CStmtBody "S2" s2Body
          (ReductionStmt
            { rsInitCode     = accInit
            , rsFinalizeCode = accStore
            , rsReductionDim = "tkb"
            })
          [("d3", "k"), ("d4", "dj")]
      ]
  , mckScratch      =
      [ ScratchDecl "acc"   "float" "TJ" 64 "tj"
      , ScratchDecl "panel" "float" "TK * TJ" 64 "tkb"
      ]
  , mckScanner      = mkPackedMultiScannerParametric
                         (fromIntegral tj) (fromIntegral tkb)
  }
  where
    prefix = case wq of WQ8 -> "q8mv_packed_"; WQ4 -> "q4mv_packed_"; WQ4K -> "q4kmv_packed_"
    funcName = prefix ++ archName arch ++ "_param"
    tj  = tileJ tiles
    tk  = tileK tiles
    tkb = tk `div` 32
    accInit = unlines
      [ "int64_t nj = TJ;"
      , "if (tj * TJ + TJ > N) nj = N - tj * TJ;"
      , "memset(acc, 0, nj * sizeof(float));"
      ]
    accStore = unlines
      [ "{ int64_t nj_ = TJ;"
      , "if (tj * TJ + TJ > N) nj_ = N - tj * TJ;"
      , "memcpy(&out[tj * TJ], acc, nj_ * sizeof(float)); }"
      ]

-- | Build a 'MultiCKernel' for the panel-packed GEMM.
-- Placeholder — to be implemented in Phase 3.
packedGemmSpec :: Arch -> Tiles -> Int -> Int -> MultiCKernel 0
packedGemmSpec = error "packedGemmSpec: not yet implemented"

-- ---------------------------------------------------------------------------
-- Typedefs (shared with Packed.hs — TODO: factor out)
-- ---------------------------------------------------------------------------

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

packedWCast :: WeightQuant -> [String]
packedWCast WQ8  = ["    const block_q8_0* W_q8 = (const block_q8_0*)W;"]
packedWCast WQ4  = ["    const block_q4_0* W_q4 = (const block_q4_0*)W;"]
packedWCast WQ4K = ["    const block_q4_K* W_q4k = (const block_q4_K*)W;"]

f16Helper :: String
f16Helper = unlines
  [ "#include <immintrin.h>"
  , "static inline float f16_to_f32(uint16_t h) {"
  , "    return _cvtsh_ss(h);"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Compilation
-- ---------------------------------------------------------------------------

-- | Compile a panel-packed matvec using polyhedral codegen.
-- Drop-in replacement for 'compilePackedMatvecWithQ' from Packed.hs.
compilePackedMatvecPoly :: WeightQuant -> Arch -> Tiles -> Int -> Int -> IO CompiledPackedMatvecPoly
compilePackedMatvecPoly wq arch tiles n kBlocks = do
  let mck = packedMatvecSpec wq arch tiles n kBlocks
      src = generateMultiC mck
  ck <- compileAndLoad (mckName mck) src
  let fn = mkMatvecFn (castFunPtr (ckFuncPtr ck))
  return CompiledPackedMatvecPoly
    { cpmpFn     = fn
    , cpmpArch   = arch
    , cpmpTiles  = tiles
    , cpmpN      = n
    , cpmpKB     = kBlocks
    , cpmpKernel = ck
    , cpmpSource = src
    }

data CompiledPackedMatvecPoly = CompiledPackedMatvecPoly
  { cpmpFn     :: !MatvecFnC
  , cpmpArch   :: !Arch
  , cpmpTiles  :: !Tiles
  , cpmpN      :: !Int
  , cpmpKB     :: !Int
  , cpmpKernel :: !CompiledKernel
  , cpmpSource :: !String
  }
