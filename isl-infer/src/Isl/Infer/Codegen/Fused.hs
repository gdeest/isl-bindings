{-# LANGUAGE DataKinds #-}

-- | Compositional fused layer code generation.
--
-- A fused layer is built by composing 'LayerAST' fragments:
--
--   * 'LPoly' — polyhedral loop nests (from 'MergedAST')
--   * 'LOpaque' — non-affine computations with trivial iteration domains
--   * 'LSeq' — sequential composition (implies barrier between phases)
--   * 'LPar' — OMP parallel region
--   * 'LBarrier' — explicit OMP barrier
--   * 'LSerial' — serial code block (runs on one thread)
--
-- The key principle: composition happens on the AST, not on C strings.
-- The C emitter sees one coherent tree and emits it in a single pass.
--
-- @
-- attnBlock = LPar
--   [ LPoly (projectionAST "Q" ...)
--   , LPoly (projectionAST "K" ...)
--   , LPoly (projectionAST "V" ...)
--   , LBarrier
--   , LSerial (ropeCode ++ attnCode)
--   , LPoly (projectionAST "O" ...)
--   ]
-- @
module Isl.Infer.Codegen.Fused
  ( -- * Layer AST
    LayerAST(..)
    -- * Building polyhedral fragments
  , projectionAST
  , attentionAST
    -- * C emission
  , emitLayerAST
  , emitLayerFunction
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Pure (PConjunction(..), mkPConjunction)
import Isl.Scan.Build (buildLevels)
import Isl.Scan.PrettyMulti (MergedAST(..), buildMergedAST)
import Isl.Scan.Multi (MultiScanner(..), ScheduledStatement(..), ULoopNest(..), StmtInverse(..))
import Isl.Infer.Arch (Tiles(..))
import Isl.Infer.Kernel.GEMM (WeightQuant(..))
import Isl.Infer.Kernel.PackedPoly (mkPackedMultiScannerParametric)
import Isl.Infer.Codegen.Multi
  ( CStmtBody(..), StmtAnnotation(..), ScratchDecl(..)
  , emitMergedAST, EmitCtx(..)
  , groupScratch, collectReductions
  )

-- ---------------------------------------------------------------------------
-- Layer AST
-- ---------------------------------------------------------------------------

-- | Compositional AST for fused layer code generation.
--
-- Composition principle: combine fragments with 'LSeq' (sequential)
-- and 'LPar' (parallel region), then emit once. No string splicing.
data LayerAST
  = LPoly PolyFragment
    -- ^ A polyhedrally-generated loop nest with statement bodies.
    -- The fragment carries its own 'EmitCtx' for self-contained emission.
  | LOpaque String String String
    -- ^ @LOpaque label dimExpr body@ — parallel for-loop with opaque body.
    -- @dimExpr@ is a C expression for the iteration count.
    -- The loop variable is @i@.
  | LSeq [LayerAST]
    -- ^ Sequential composition. Semantically implies a data dependency
    -- (and thus a barrier) between adjacent elements within a parallel region.
  | LPar [LayerAST]
    -- ^ OMP parallel region: wraps content in @#pragma omp parallel { ... }@.
  | LBarrier
    -- ^ Explicit @#pragma omp barrier@.
  | LSerial String
    -- ^ Serial code block. In a parallel region, emitted as
    -- @#pragma omp single nowait { ... }@ or just raw C outside a region.
  | LComment String
    -- ^ A comment line (for readability of generated code).

-- | A self-contained polyhedral loop nest fragment, ready for C emission.
data PolyFragment = PolyFragment
  { pfLabel     :: !String
  , pfAST       :: !MergedAST     -- ^ The merged loop structure
  , pfCtx       :: !EmitCtx       -- ^ Emission context (dim names, stmt bodies, scratch, etc.)
  , pfNowait    :: !Bool          -- ^ Use @nowait@ on the outermost @omp for@
  }

-- ---------------------------------------------------------------------------
-- Building projection fragments
-- ---------------------------------------------------------------------------

-- | Build a polyhedral projection fragment.
--
-- Uses parametric domains (N and KB are ISL parameters that map to
-- the given C expression names). Tile sizes are baked into the domain
-- constraints as integer coefficients.
--
-- The returned 'LayerAST' can be composed with other fragments via 'LSeq'
-- and 'LPar' without touching any C strings.
projectionAST
  :: String       -- ^ Label (e.g. "Q", "Gate")
  -> WeightQuant  -- ^ Weight quantization
  -> Int          -- ^ tileJ
  -> Int          -- ^ tileKB (in Q8 blocks)
  -> String       -- ^ C output variable name
  -> String       -- ^ C input variable name
  -> String       -- ^ C weight variable name (already cast)
  -> String       -- ^ C expression for N (e.g. "DIM", "KVD")
  -> String       -- ^ C expression for KB (e.g. "KBD", "KBH")
  -> Bool         -- ^ nowait
  -> LayerAST
projectionAST label wq tj tkb outVar inpVar wVar nExpr kbExpr nowait =
  let timeDims = ["tj", "tkb", "seq", "d3", "d4", "d5"]
      -- Build the parametric MultiScanner
      scanner = mkPackedMultiScannerParametric (fromIntegral tj) (fromIntegral tkb)
      -- Build the MergedAST from the scanner
      ast = buildMergedAST timeDims scanner
      -- Build the EmitCtx
      stmtBodies =
        [ CStmtBody "S1" (s1BodyFor wq tj tkb wVar kbExpr) PlainStmt
            [("d3", "dj"), ("d4", "kb"), ("d5", "v")]
        , CStmtBody "S2" (s2BodyFor tj tkb inpVar)
            (ReductionStmt
              { rsInitCode     = accInitFor tj nExpr
              , rsFinalizeCode = accStoreFor tj outVar nExpr
              , rsReductionDim = "tkb"
              })
            [("d3", "k"), ("d4", "dj")]
        ]
      scratchDecls =
        [ ScratchDecl "acc"   "float" (show tj) 64 "tj"
        , ScratchDecl "panel" "float" (show (tj * tkb * 32)) 64 "tkb"
        ]
      ctx = EmitCtx
        { ecDimNames     = timeDims
        , ecParamNames   = [kbExpr, nExpr]  -- ISL params alphabetical: KB < N
        , ecParallelDims = Set.singleton 0  -- tj is parallel
        , ecSimdDims     = Set.empty
        , ecStmtBodies   = Map.fromList [(csbName b, b) | b <- stmtBodies]
        , ecScratch      = groupScratch scratchDecls
        , ecReductions   = collectReductions stmtBodies
        , ecDimNameToIdx = Map.fromList (zip timeDims [0..])
        , ecNestedParallel = True  -- projections live inside a LPar region
        , ecNowait = nowait
        }
  in LPoly (PolyFragment label ast ctx nowait)

-- ---------------------------------------------------------------------------
-- Attention fragment
-- ---------------------------------------------------------------------------

-- | Polyhedral domain for tiled multi-head attention:
--
-- @[NH, SL] -> { [h, t0] : 0 <= h < NH, 0 <= t0, TILE * t0 <= SL - 1 }@
--
-- @h@ is parallel (across heads), @t0@ is sequential (carries softmax state).
-- @TILE@ (attention tile size) is baked as an integer coefficient.
attnDomain :: Integer -> PConjunction '["NH", "SL"] 2
attnDomain tile = mkPConjunction @'["NH","SL"] @2 $
  \(nhp :- slp :- Nil) (d_h :- d_t0 :- Nil) ->
    -- h: 0 <= h < NH
        idx d_h >=: cst 0
    &&: idx d_h <=: idx nhp -: cst 1
    -- t0: 0 <= t0, TILE*t0 <= SL-1
    &&: idx d_t0 >=: cst 0
    &&: tile *: idx d_t0 <=: idx slp -: cst 1

-- | Build a parametric MultiScanner for the attention (h, t0) loop.
mkAttnScanner :: Integer -> MultiScanner 2
mkAttnScanner tile =
  let PConjunction conj = attnDomain tile
      (levels, nParams, nDims) = buildLevels (let Conjunction cs = conj in cs)
      nest = ULoopNest levels nParams nDims
  in MultiScanner
    [ ScheduledStatement "attn" [nest] (StmtInverse 2 []) ]

-- | Build a polyhedral attention fragment.
--
-- The @(h, t0)@ loop is ISL-generated. @h@ is parallel (omp for).
-- The per-tile body (QK^T + online softmax + V accumulation) is opaque.
--
-- The softmax state is a reduction over @t0@: init before the tile loop,
-- finalize (divide by l_prev) after it.
--
-- @tile@ is the attention tile size (e.g. 64).
-- @nhExpr@ and @slExpr@ are C expressions for NH and sl (e.g. @\"NH\"@, @\"sl\"@).
-- @tileBody@ is the C code for one tile iteration (references @h@, @t0@).
attentionAST
  :: Int          -- ^ Attention tile size (e.g. 64)
  -> String       -- ^ C expression for NH (e.g. "NH")
  -> String       -- ^ C expression for sl (e.g. "sl")
  -> String       -- ^ Per-head init code (before t0 loop)
  -> String       -- ^ Per-tile body (QK^T + softmax + V accum)
  -> String       -- ^ Per-head finalize code (after t0 loop)
  -> LayerAST
attentionAST tile nhExpr slExpr initCode tileBody finalizeCode =
  let timeDims = ["h", "t0"]
      scanner = mkAttnScanner (fromIntegral tile)
      ast = buildMergedAST timeDims scanner
      stmtBodies =
        [ CStmtBody "attn" tileBody
            (ReductionStmt
              { rsInitCode     = initCode
              , rsFinalizeCode = finalizeCode
              , rsReductionDim = "t0"
              })
            []  -- no aliases needed, body uses h and t0 directly
        ]
      ctx = EmitCtx
        { ecDimNames     = timeDims
        , ecParamNames   = [nhExpr, slExpr]  -- ISL params alphabetical: NH < SL
        , ecParallelDims = Set.singleton 0  -- h is parallel
        , ecSimdDims     = Set.empty
        , ecStmtBodies   = Map.fromList [(csbName b, b) | b <- stmtBodies]
        , ecScratch      = Map.empty
        , ecReductions   = collectReductions stmtBodies
        , ecDimNameToIdx = Map.fromList (zip timeDims [0..])
        , ecNestedParallel = True  -- inside LPar
        , ecNowait = False  -- attention needs barrier (implicit from omp for)
        }
  in LPoly (PolyFragment "attention" ast ctx False)

-- ---------------------------------------------------------------------------
-- C emission
-- ---------------------------------------------------------------------------

-- | Emit a 'LayerAST' as C code lines at the given indentation depth.
emitLayerAST :: Int -> LayerAST -> [String]

emitLayerAST depth (LPoly pf) =
  -- Each projection gets its own { } scope to avoid variable name clashes
  -- (multiple projections in the same parallel region emit tj_hi, etc.)
  -- Context-aware: ecNestedParallel in the EmitCtx ensures "omp for" (not "parallel for")
  let cLines = emitMergedAST (pfCtx pf) (depth + 1) False (pfAST pf)
  in [ ind depth ++ "{ /* " ++ pfLabel pf ++ " projection — polyhedral */" ]
     ++ cLines
     ++ [ ind depth ++ "}" ]

emitLayerAST depth (LOpaque label dimExpr body) =
  [ ind depth ++ "/* " ++ label ++ " */"
  , ind depth ++ "#pragma omp for schedule(static)"
  , ind depth ++ "for (int64_t i = 0; i < " ++ dimExpr ++ "; i++) {"
  ]
  ++ [ind (depth + 1) ++ l | l <- lines body, not (null l)]
  ++ [ind depth ++ "}"]

emitLayerAST depth (LSeq blocks) =
  concatMap (emitLayerAST depth) blocks

emitLayerAST depth (LPar blocks) =
  [ ind depth ++ "#pragma omp parallel"
  , ind depth ++ "{"
  ]
  ++ concatMap (emitLayerAST (depth + 1)) blocks
  ++ [ind depth ++ "}"]

emitLayerAST depth LBarrier =
  [ind depth ++ "#pragma omp barrier"]

emitLayerAST depth (LSerial code) =
  [ ind depth ++ "#pragma omp single nowait"
  , ind depth ++ "{"
  ]
  ++ [ind (depth + 1) ++ l | l <- lines code, not (null l)]
  ++ [ind depth ++ "}"]

emitLayerAST depth (LComment msg) =
  [ind depth ++ "/* " ++ msg ++ " */"]

-- | Emit a complete C function wrapping a 'LayerAST'.
emitLayerFunction :: String -> [(String, String)] -> String -> LayerAST -> String -> String
emitLayerFunction funcName params preamble body postamble = unlines $
  [ "void " ++ funcName ++ "(" ++ renderParams params ++ ") {"
  ]
  ++ [ind 1 ++ l | l <- lines preamble, not (null l)]
  ++ emitLayerAST 1 body
  ++ [ind 1 ++ l | l <- lines postamble, not (null l)]
  ++ ["}"]

-- ---------------------------------------------------------------------------
-- Statement body builders (parameterized by C variable names)
-- ---------------------------------------------------------------------------

-- All bodies use concrete tile sizes as literal integers — no macro dependency.
-- This makes projections self-contained and composable.

s1BodyFor :: WeightQuant -> Int -> Int -> String -> String -> String
s1BodyFor WQ8 tj tkb wVar kbExpr = unlines
  [ "{"
  , "    int64_t j = tj * " ++ show tj ++ " + dj;"
  , "    const block_q8_0* blk = &" ++ wVar ++ "[j * " ++ kbExpr ++ " + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    int64_t base = (kb - tkb * " ++ show tkb ++ ") * 32;"
  , "    panel[(base + v) * " ++ show tj ++ " + dj] = scale * (float)blk->qs[v];"
  , "}"
  ]
s1BodyFor WQ4 tj tkb wVar kbExpr = unlines
  [ "{"
  , "    int64_t j = tj * " ++ show tj ++ " + dj;"
  , "    const block_q4_0* blk = &" ++ wVar ++ "[j * " ++ kbExpr ++ " + kb];"
  , "    float scale = f16_to_f32(blk->d);"
  , "    int64_t base = (kb - tkb * " ++ show tkb ++ ") * 32;"
  , "    int nibble = (blk->qs[v / 2] >> ((v & 1) * 4)) & 0xF;"
  , "    panel[(base + v) * " ++ show tj ++ " + dj] = scale * (float)(nibble - 8);"
  , "}"
  ]
s1BodyFor WQ4K tj tkb wVar kbExpr = unlines
  [ "{"
  , "    int64_t j = tj * " ++ show tj ++ " + dj;"
  , "    int64_t sb = kb / 8; int64_t si = kb & 7;"
  , "    const block_q4_K* sblk = &" ++ wVar ++ "[j * (" ++ kbExpr ++ " / 8) + sb];"
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
  , "    int64_t base = (kb - tkb * " ++ show tkb ++ ") * 32;"
  , "    int nibble = (sblk->qs[si*16 + v/2] >> ((v&1)*4)) & 0xF;"
  , "    panel[(base + v) * " ++ show tj ++ " + dj] = d_sc * (float)nibble - d_mn;"
  , "}"
  ]

s2BodyFor :: Int -> Int -> String -> String
s2BodyFor tj tkb inpVar = unlines
  [ "{"
  , "    float xk = " ++ inpVar ++ "[(tkb * " ++ show tkb ++ " * 32) + k];"
  , "    acc[dj] += xk * panel[k * " ++ show tj ++ " + dj];"
  , "}"
  ]

accInitFor :: Int -> String -> String
accInitFor tj nExpr = unlines
  [ "int64_t nj = " ++ show tj ++ ";"
  , "if (tj * " ++ show tj ++ " + " ++ show tj ++ " > " ++ nExpr ++ ") nj = " ++ nExpr ++ " - tj * " ++ show tj ++ ";"
  , "memset(acc, 0, nj * sizeof(float));"
  ]

accStoreFor :: Int -> String -> String -> String
accStoreFor tj outVar nExpr = unlines
  -- nj was already declared in init — just recompute (same value, avoids redeclaration)
  [ "{ int64_t nj_ = " ++ show tj ++ ";"
  , "if (tj * " ++ show tj ++ " + " ++ show tj ++ " > " ++ nExpr ++ ") nj_ = " ++ nExpr ++ " - tj * " ++ show tj ++ ";"
  , "memcpy(&" ++ outVar ++ "[tj * " ++ show tj ++ "], acc, nj_ * sizeof(float)); }"
  ]

ind :: Int -> String
ind n = replicate (n * 4) ' '

renderParams :: [(String, String)] -> String
renderParams [] = "void"
renderParams ps = go ps
  where
    go [] = ""
    go [x] = pair x
    go (x:xs) = pair x ++ ", " ++ go xs
    pair (t, n) = t ++ " " ++ n

