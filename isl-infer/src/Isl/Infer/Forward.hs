{-# LANGUAGE RecordWildCards #-}

-- | Full Llama forward pass with fused per-layer kernel.
--
-- == Decode (single-token)
-- Uses the fused per-layer kernel: one C function per layer with
-- inlined matvec projections and attention.
--
-- == Prefill (batched)
-- Uses standalone polyhedral Q8 GEMM kernels for weight projections
-- (the dominant cost), with Haskell-side orchestration for elementwise
-- ops (RoPE, RMSNorm, attention, SiLU — all cheap relative to GEMM).
-- OMP overhead is negligible at B>1 since each GEMM does B× more work.
module Isl.Infer.Forward
  ( InferState(..)
  , initInferState
  , forward
  , forwardBatch
  , forwardBatchAllLogits
  , generateToken
  , downquantFFNWeights
    -- * Draft model speculative decoding
  , draftTokens
  ) where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Word (Word8, Word64)
import qualified Data.Map.Strict as Map
import Foreign.C.Types (CLong(..))
import Foreign.Marshal.Alloc (mallocBytes, callocBytes, free)
import Foreign.Marshal.Array (mallocArray, peekArray, pokeArray)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (poke)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import Isl.Infer.GGUF
import Isl.Infer.Model
import Isl.Infer.Compile
import Isl.Infer.Kernel.Packed (callPackedGemm)
import Isl.Infer.Downquant (downquantFFN)
import Isl.Infer.Schedule (KVCacheMode(..), KVQuant(..))
import Isl.Infer.Kernel.GEMM (CompiledMatvec(..), CompiledQ8Gemm(..), callQ8Gemm)
import Isl.Infer.Kernel.FusedLayer (CompiledLayer(..))
import Isl.Infer.Kernel.Elementwise

foreign import ccall unsafe "omp_get_max_threads" ompGetMaxThreads :: IO Int

foreign import ccall unsafe "clock_gettime"
  c_clock_gettime_fwd :: Int -> Ptr () -> IO Int

getTimeNsFwd :: IO Word64
getTimeNsFwd = do
  buf <- mallocArray 2 :: IO (Ptr CLong)
  _ <- c_clock_gettime_fwd 1 (castPtr buf)
  [sec, nsec] <- peekArray 2 buf
  free buf
  return (fromIntegral sec * 1000000000 + fromIntegral nsec)

timeOp :: String -> IO () -> IO ()
timeOp label act = do
  t0 <- getTimeNsFwd
  act
  t1 <- getTimeNsFwd
  let ms = fromIntegral (t1 - t0) / 1e6 :: Double
  printf "    %-20s %7.1f ms\n" label ms
  hFlush stdout

-- | Inference state.
data InferState = InferState
  { isConfig   :: !LlamaConfig
  , isGGUF     :: !GGUFFile
  , isBasePtr  :: !(Ptr Word8)
  , isKernels  :: !KernelSet
    -- Scratch buffers
  , isX        :: !(Ptr Float)
  , isXB       :: !(Ptr Float)
  , isXB2      :: !(Ptr Float)
  , isQ        :: !(Ptr Float)
  , isK        :: !(Ptr Float)
  , isV        :: !(Ptr Float)
  , isHB       :: !(Ptr Float)
  , isHB2      :: !(Ptr Float)
  , isLogits   :: !(Ptr Float)
  , isAttnOut  :: !(Ptr Float)
  , isScores   :: !(Ptr Float)    -- [n_heads * max_seq] for head-parallel attention
    -- Prefill batch buffers (pre-allocated for B_MAX tokens)
  , isPfX      :: !(Ptr Float)   -- [B_MAX, dim]
  , isPfXB     :: !(Ptr Float)   -- [B_MAX, dim]
  , isPfXB2    :: !(Ptr Float)   -- [B_MAX, dim]
  , isPfQ      :: !(Ptr Float)   -- [B_MAX, dim]
  , isPfK      :: !(Ptr Float)   -- [B_MAX, kv_dim]
  , isPfV      :: !(Ptr Float)   -- [B_MAX, kv_dim]
  , isPfHB     :: !(Ptr Float)   -- [B_MAX, hidden]
  , isPfHB2    :: !(Ptr Float)   -- [B_MAX, hidden]
  , isPfAttnOut :: !(Ptr Float)  -- [B_MAX, dim]
    -- KV cache
  , isKCache   :: !(Ptr Float)
  , isVCache   :: !(Ptr Float)
  , isKVMode   :: !KVCacheMode
    -- RoPE precomputed
  , isInvFreq  :: !(Ptr Float)
    -- Speculative decode
  , isSpecLogits :: !(Ptr Float)  -- ^ @[specK, vocab]@ logits for batch verification
  , isSpecK      :: !Int          -- ^ Max speculation depth (0 = disabled)
    -- RNG
  , isRNG      :: !(Ptr Word64)
    -- Weight overlay: downquantized weights take precedence over mmap'd pointers
  , isWeightOverlay :: !(Map.Map String (Ptr Word8))
  }

initInferState :: KVCacheMode -> LlamaConfig -> GGUFFile -> KernelSet -> IO InferState
initInferState kvMode cfg gf kernels = do
  let d   = lcDim cfg
      hd  = lcHiddenDim cfg
      vs  = lcVocabSize cfg
      ms  = lcMaxSeqLen cfg
      kvd = lcKVDim cfg
      nl  = lcNLayers cfg
      nh  = lcNHeads cfg
      headDim = lcHeadDim cfg
      f4  = 4

  basePtr <- withForeignPtr (ggufBasePtr gf) return

  x       <- mallocBytes (d * f4)
  xb      <- mallocBytes (d * f4)
  xb2     <- mallocBytes (d * f4)
  q       <- mallocBytes (d * f4)
  k       <- mallocBytes (kvd * f4)
  v       <- mallocBytes (kvd * f4)
  hb      <- mallocBytes (hd * f4)
  hb2     <- mallocBytes (hd * f4)
  logits  <- mallocBytes (vs * f4)
  attnOut <- mallocBytes (d * f4)
  maxT    <- ompGetMaxThreads
  let scoreDim = max nh maxT
  scores  <- mallocBytes (scoreDim * ms * f4) -- per-head (decode) / per-thread (prefill)
  let nkv = lcNKVHeads cfg
      headDim' = lcHeadDim cfg
      kBytesPerLayer = quantLayerBytes nkv ms headDim' (kvKQuant kvMode)
      vBytesPerLayer = quantLayerBytes nkv ms headDim' (kvVQuant kvMode)
  putStrLn $ "  KV cache: K=" ++ show (kvKQuant kvMode) ++ " ("
           ++ show (nl * kBytesPerLayer `div` (1024*1024)) ++ " MB), V="
           ++ show (kvVQuant kvMode) ++ " ("
           ++ show (nl * vBytesPerLayer `div` (1024*1024)) ++ " MB)"
  kCache  <- callocBytes (nl * kBytesPerLayer)
  vCache  <- callocBytes (nl * vBytesPerLayer)

  -- Prefill batch buffers
  let bMax = prefillBatchMax
  pfX      <- mallocBytes (bMax * d   * f4)
  pfXB     <- mallocBytes (bMax * d   * f4)
  pfXB2    <- mallocBytes (bMax * d   * f4)
  pfQ      <- mallocBytes (bMax * d   * f4)
  pfK      <- mallocBytes (bMax * kvd * f4)
  pfV      <- mallocBytes (bMax * kvd * f4)
  pfHB     <- mallocBytes (bMax * hd  * f4)
  pfHB2    <- mallocBytes (bMax * hd  * f4)
  pfAttnOut <- mallocBytes (bMax * d  * f4)

  -- Precompute RoPE inverse frequencies
  let half = headDim `div` 2
      freqBase = lcRopeFreqBase cfg
  invFreq <- mallocBytes (half * f4)
  pokeArray invFreq [ 1.0 / (freqBase ** (2.0 * fromIntegral i / fromIntegral headDim))
                    | i <- [0 .. half - 1] :: [Int] ]

  -- Speculative decode buffer (K_MAX tokens × vocab logits)
  let specK = 8  -- max speculation depth (runtime K can be smaller)
  specLogits <- callocBytes (specK * vs * f4)

  rng <- mallocBytes 8
  poke rng (42 :: Word64)

  return InferState
    { isConfig = cfg, isGGUF = gf, isBasePtr = basePtr, isKernels = kernels
    , isX = x, isXB = xb, isXB2 = xb2
    , isQ = q, isK = k, isV = v
    , isHB = hb, isHB2 = hb2
    , isLogits = logits, isAttnOut = attnOut, isScores = scores
    , isPfX = pfX, isPfXB = pfXB, isPfXB2 = pfXB2
    , isPfQ = pfQ, isPfK = pfK, isPfV = pfV
    , isPfHB = pfHB, isPfHB2 = pfHB2, isPfAttnOut = pfAttnOut
    , isKCache = kCache, isVCache = vCache, isKVMode = kvMode
    , isSpecLogits = specLogits, isSpecK = specK
    , isInvFreq = invFreq, isRNG = rng
    , isWeightOverlay = Map.empty
    }

-- | Raw tensor pointer. Checks the weight overlay first (for downquantized weights),
-- then falls back to the mmap'd GGUF data.
tensorPtr :: InferState -> String -> Ptr Word8
tensorPtr is name =
  case Map.lookup name (isWeightOverlay is) of
    Just ptr -> ptr
    Nothing ->
      case Map.lookup name (ggufTensors (isGGUF is)) of
        Just ti -> isBasePtr is `plusPtr` (ggufDataOffset (isGGUF is) + fromIntegral (tiOffset ti))
        Nothing -> error $ "tensorPtr: " ++ name ++ " not found"

-- | Output projection weight pointer. Handles weight tying: Llama 3.x models
-- reuse token_embd.weight for the output projection (no separate output.weight).
outputWeightPtr :: InferState -> Ptr Word8
outputWeightPtr is =
  case Map.lookup "output.weight" (ggufTensors (isGGUF is)) of
    Just ti -> isBasePtr is `plusPtr` (ggufDataOffset (isGGUF is) + fromIntegral (tiOffset ti))
    Nothing -> tensorPtr is "token_embd.weight"  -- weight tying fallback

layerW :: InferState -> Int -> String -> Ptr Word8
layerW is l suffix = tensorPtr is ("blk." ++ show l ++ "." ++ suffix)

-- | Bytes per layer for a single KV cache tensor at a given quantization.
-- Layout: [n_kv_heads, max_seq, head_dim] (always transposed).
quantLayerBytes :: Int -> Int -> Int -> KVQuant -> Int
quantLayerBytes nkv ms hd QFloat32 = nkv * ms * hd * 4
quantLayerBytes nkv ms hd QQ8      = nkv * ms * (hd `div` 32) * 34
quantLayerBytes nkv ms hd QQ4      = nkv * ms * (hd `div` 32) * 18

-- | Forward pass for one token at position pos.
-- Uses the fused per-layer kernel for all transformer layers.
forward :: InferState -> Int -> Int -> IO ()
forward is token pos = do
  let cfg  = isConfig is
      dim  = lcDim cfg
      nl   = lcNLayers cfg
      kvd  = lcKVDim cfg
      ms   = lcMaxSeqLen cfg
      nkv  = lcNKVHeads cfg
      hd   = lcHeadDim cfg
      fi   = fromIntegral
      fusedFn = clFn (ksFusedLayer (isKernels is))
      kLayerBytes = quantLayerBytes nkv ms hd (kvKQuant (isKVMode is))
      vLayerBytes = quantLayerBytes nkv ms hd (kvVQuant (isKVMode is))

  -- 1. Token embedding
  embedToken (isX is) (tensorPtr is "token_embd.weight") (fi token) (fi dim)

  -- 2. Transformer layers via fused kernel
  mapM_ (\l -> do
    let kcLayer = isKCache is `plusPtr` (l * kLayerBytes)
        vcLayer = isVCache is `plusPtr` (l * vLayerBytes)
    fusedFn
      (isX is) (isXB is) (isXB2 is)
      (isQ is) (isK is) (isV is)
      (isHB is) (isHB2 is) (isAttnOut is) (isScores is)
      (castPtr kcLayer) (castPtr vcLayer)
      (layerW is l "attn_q.weight")
      (layerW is l "attn_k.weight")
      (layerW is l "attn_v.weight")
      (layerW is l "attn_output.weight")
      (layerW is l "ffn_gate.weight")
      (layerW is l "ffn_up.weight")
      (layerW is l "ffn_down.weight")
      (castPtr (layerW is l "attn_norm.weight"))
      (castPtr (layerW is l "ffn_norm.weight"))
      (isInvFreq is)
      (fi pos)
    ) [0..nl-1]

  -- 3. Final norm + output projection (not fused — only runs once)
  rmsnorm (isX is) (isX is) (castPtr (tensorPtr is "output_norm.weight")) (fi dim) (lcRMSNormEps cfg)
  _ <- c_memset (castPtr (isLogits is)) 0 (fromIntegral (lcVocabSize cfg * 4))
  callMatvec (ksOutput (isKernels is)) (isLogits is) (isX is) (outputWeightPtr is)

-- ---------------------------------------------------------------------------
-- Batched forward pass (prefill)
-- ---------------------------------------------------------------------------

-- | Forward pass for a batch of B tokens starting at position startPos.
--
-- Uses standalone polyhedral Q8 GEMM kernels for weight projections
-- and batched C helpers for elementwise ops (1 FFI call per op per layer,
-- not B*NH calls).
--
-- Per layer: 7 GEMM + 7 elementwise = 14 FFI calls (vs ~7000 before).
--
-- After this call, isLogits contains the logits for the LAST token only.
forwardBatch :: InferState -> [Int] -> Int -> IO ()
forwardBatch is tokens startPos = do
  let cfg   = isConfig is
      dim   = lcDim cfg
      hDim  = lcHiddenDim cfg
      nl    = lcNLayers cfg
      kvd   = lcKVDim cfg
      ms    = lcMaxSeqLen cfg
      nh    = lcNHeads cfg
      nkv   = lcNKVHeads cfg
      hdim  = lcHeadDim cfg
      b     = length tokens
      fi    = fromIntegral
      f4    = 4
      ks    = isKernels is
      kLayerBytes = quantLayerBytes nkv ms hdim (kvKQuant (isKVMode is))
      vLayerBytes = quantLayerBytes nkv ms hdim (kvVQuant (isKVMode is))

  -- 1. Embed all B tokens into isPfX [B, dim]
  mapM_ (\(i, tok) ->
    embedToken (isPfX is `plusPtr` (i * dim * f4))
               (tensorPtr is "token_embd.weight")
               (fi tok) (fi dim)
    ) (zip [0..] tokens)

  -- 2. Transformer layers — 14 FFI calls per layer
  mapM_ (\l -> do
    let kcLayer = isKCache is `plusPtr` (l * kLayerBytes)
        vcLayer = isVCache is `plusPtr` (l * vLayerBytes)
        doTime = l == 0  -- time first layer only
        timed lbl act = if doTime then timeOp lbl act else act

    when doTime $ printf "  Layer 0 timing (B=%d):\n" b >> hFlush stdout

    -- ---- ATTENTION BLOCK ----
    timed "rmsnorm_attn" $
      rmsnormBatch (isPfXB is) (isPfX is)
        (castPtr (layerW is l "attn_norm.weight"))
        (fi dim) (lcRMSNormEps cfg) (fi b)

    timed "gemm_Q" $
      callQ8Gemm (ksPfQProj ks) (isPfQ is) (isPfXB is) (layerW is l "attn_q.weight") b
    timed "gemm_K" $
      callQ8Gemm (ksPfKProj ks) (isPfK is) (isPfXB is) (layerW is l "attn_k.weight") b
    timed "gemm_V" $
      callQ8Gemm (ksPfVProj ks) (isPfV is) (isPfXB is) (layerW is l "attn_v.weight") b

    timed "rope" $
      ropeBatch (isPfQ is) (isPfK is) (isInvFreq is)
        (fi nh) (fi nkv) (fi hdim) (fi dim) (fi kvd) (fi startPos) (fi b)

    timed "kv_cache_write" $
      kvCacheWriteBatch (castPtr kcLayer) (castPtr vcLayer) (isPfK is) (isPfV is)
        (fi nkv) (fi hdim) (fi ms) (fi kvd) (fi startPos) (fi b)

    timed "attention" $
      attentionPrefillBatch (isPfAttnOut is) (isPfQ is)
        (castPtr kcLayer) (castPtr vcLayer)
        (isScores is) (fi nh) (fi nkv) (fi hdim) (fi ms)
        (fi dim) (fi startPos) (fi b)

    timed "gemm_O" $
      callQ8Gemm (ksPfOProj ks) (isPfXB2 is) (isPfAttnOut is) (layerW is l "attn_output.weight") b

    timed "residual_attn" $
      residualAddBatch (isPfX is) (isPfX is) (isPfXB2 is) (fi dim) (fi b)

    -- ---- FFN BLOCK ----
    timed "rmsnorm_ffn" $
      rmsnormBatch (isPfXB is) (isPfX is)
        (castPtr (layerW is l "ffn_norm.weight"))
        (fi dim) (lcRMSNormEps cfg) (fi b)

    timed "gemm_gate" $
      callQ8Gemm (ksPfGate ks) (isPfHB  is) (isPfXB is) (layerW is l "ffn_gate.weight") b
    timed "gemm_up" $
      callQ8Gemm (ksPfUp   ks) (isPfHB2 is) (isPfXB is) (layerW is l "ffn_up.weight") b

    timed "silu_mul" $
      siluMulBatch (isPfHB is) (isPfHB is) (isPfHB2 is) (fi hDim) (fi b)

    timed "gemm_down" $
      callQ8Gemm (ksPfDown ks) (isPfXB2 is) (isPfHB is) (layerW is l "ffn_down.weight") b

    timed "residual_ffn" $
      residualAddBatch (isPfX is) (isPfX is) (isPfXB2 is) (fi dim) (fi b)
    ) [0..nl-1]

  -- 3. Final norm + output projection on LAST token only
  let lastOff = (b - 1) * dim * f4
  -- Copy last token's embedding to isX for decode continuity
  memcpyFloat (isX is) (isPfX is `plusPtr` lastOff) (fi dim)
  rmsnorm (isX is) (isX is) (castPtr (tensorPtr is "output_norm.weight")) (fi dim) (lcRMSNormEps cfg)
  _ <- c_memset (castPtr (isLogits is)) 0 (fromIntegral (lcVocabSize cfg * 4))
  callMatvec (ksOutput (isKernels is)) (isLogits is) (isX is) (outputWeightPtr is)

-- | Copy n floats between pointers.
memcpyFloat :: Ptr Float -> Ptr Float -> Int64 -> IO ()
memcpyFloat dst src n = do
  _ <- c_memcpy (castPtr dst) (castPtr src) (fromIntegral n * 4)
  return ()

foreign import ccall unsafe "memcpy"
  c_memcpy :: Ptr () -> Ptr () -> Int64 -> IO (Ptr ())

foreign import ccall unsafe "memset"
  c_memset :: Ptr () -> Int -> Int64 -> IO (Ptr ())

-- ---------------------------------------------------------------------------
-- Batched forward pass with ALL-token logits (speculative decode verify)
-- ---------------------------------------------------------------------------

-- | Like 'forwardBatch' but computes logits for ALL B tokens, not just
-- the last. Logits for token @b@ are stored at
-- @isSpecLogits + b * vocabSize@ (floats).
--
-- Also copies the LAST token's embedding to 'isX' for decode continuity,
-- matching 'forwardBatch' behavior.
forwardBatchAllLogits :: InferState -> [Int] -> Int -> IO ()
forwardBatchAllLogits is tokens startPos = do
  let cfg   = isConfig is
      dim   = lcDim cfg
      nl    = lcNLayers cfg
      kvd   = lcKVDim cfg
      hDim  = lcHiddenDim cfg
      ms    = lcMaxSeqLen cfg
      nh    = lcNHeads cfg
      nkv   = lcNKVHeads cfg
      hdim  = lcHeadDim cfg
      vs    = lcVocabSize cfg
      b     = length tokens
      fi    = fromIntegral
      f4    = 4
      ks    = isKernels is
      kLayerBytes = quantLayerBytes nkv ms hdim (kvKQuant (isKVMode is))
      vLayerBytes = quantLayerBytes nkv ms hdim (kvVQuant (isKVMode is))

  -- 1. Embed all B tokens into isPfX [B, dim]
  mapM_ (\(i, tok) ->
    embedToken (isPfX is `plusPtr` (i * dim * f4))
               (tensorPtr is "token_embd.weight")
               (fi tok) (fi dim)
    ) (zip [0..] tokens)

  -- 2. Transformer layers (identical to forwardBatch)
  mapM_ (\l -> do
    let kcLayer = isKCache is `plusPtr` (l * kLayerBytes)
        vcLayer = isVCache is `plusPtr` (l * vLayerBytes)

    -- Attention block
    rmsnormBatch (isPfXB is) (isPfX is)
      (castPtr (layerW is l "attn_norm.weight"))
      (fi dim) (lcRMSNormEps cfg) (fi b)

    callPackedGemm (ksPkQProj ks) (isPfQ is) (isPfXB is) (layerW is l "attn_q.weight") b
    callPackedGemm (ksPkKProj ks) (isPfK is) (isPfXB is) (layerW is l "attn_k.weight") b
    callPackedGemm (ksPkVProj ks) (isPfV is) (isPfXB is) (layerW is l "attn_v.weight") b

    ropeBatch (isPfQ is) (isPfK is) (isInvFreq is)
      (fi nh) (fi nkv) (fi hdim) (fi dim) (fi kvd) (fi startPos) (fi b)

    kvCacheWriteBatch (castPtr kcLayer) (castPtr vcLayer) (isPfK is) (isPfV is)
      (fi nkv) (fi hdim) (fi ms) (fi kvd) (fi startPos) (fi b)

    attentionPrefillBatch (isPfAttnOut is) (isPfQ is)
      (castPtr kcLayer) (castPtr vcLayer)
      (isScores is) (fi nh) (fi nkv) (fi hdim) (fi ms)
      (fi dim) (fi startPos) (fi b)

    callPackedGemm (ksPkOProj ks) (isPfXB2 is) (isPfAttnOut is) (layerW is l "attn_output.weight") b

    residualAddBatch (isPfX is) (isPfX is) (isPfXB2 is) (fi dim) (fi b)

    -- FFN block
    rmsnormBatch (isPfXB is) (isPfX is)
      (castPtr (layerW is l "ffn_norm.weight"))
      (fi dim) (lcRMSNormEps cfg) (fi b)

    callPackedGemm (ksPkGate ks) (isPfHB  is) (isPfXB is) (layerW is l "ffn_gate.weight") b
    callPackedGemm (ksPkUp   ks) (isPfHB2 is) (isPfXB is) (layerW is l "ffn_up.weight") b

    siluMulBatch (isPfHB is) (isPfHB is) (isPfHB2 is) (fi hDim) (fi b)

    callPackedGemm (ksPkDown ks) (isPfXB2 is) (isPfHB is) (layerW is l "ffn_down.weight") b

    residualAddBatch (isPfX is) (isPfX is) (isPfXB2 is) (fi dim) (fi b)
    ) [0..nl-1]

  -- 3. Final norm + output projection for ALL B tokens → isSpecLogits
  --    IMPORTANT: callMatvec accumulates (out[j] += ...), so we must zero
  --    each output region before calling.
  let outNorm = castPtr (tensorPtr is "output_norm.weight")
      outW    = outputWeightPtr is
  mapM_ (\i -> do
    let srcOff = i * dim * f4
        dstOff = i * vs * f4
    -- Zero the output region (callMatvec accumulates, not assigns)
    _ <- c_memset (castPtr (isSpecLogits is `plusPtr` dstOff)) 0 (fromIntegral (vs * f4))
    -- RMSNorm token i into isPfXB (reuse as scratch)
    rmsnorm (isPfXB is) (isPfX is `plusPtr` srcOff) outNorm (fi dim) (lcRMSNormEps cfg)
    -- Output projection → specLogits[i]
    callMatvec (ksOutput ks) (isSpecLogits is `plusPtr` dstOff) (isPfXB is) outW
    ) [0..b-1]

  -- Copy last token's embedding to isX for decode continuity
  let lastOff = (b - 1) * dim * f4
  memcpyFloat (isX is) (isPfX is `plusPtr` lastOff) (fi dim)

-- | Generate one token.
generateToken :: InferState -> Int -> Int -> Float -> Float -> IO Int
generateToken is token pos temperature topP = do
  forward is token pos
  tok <- sampleTopP (isLogits is) (fromIntegral (lcVocabSize (isConfig is))) temperature topP (isRNG is)
  return (fromIntegral tok)

-- | Generate K draft tokens using a draft model (smaller InferState).
--
-- The draft model runs K sequential forward passes with greedy sampling
-- (temperature=0, topP=1). Its KV cache is updated but can be rolled back
-- if verification rejects tokens.
--
-- Returns the list of drafted token IDs (may be fewer than K if EOS is hit).
-- | Generate K draft tokens using a draft model.
--
-- IMPORTANT: The draft model must already have been forwarded with the
-- previous token — its isLogits already predict the next position.
-- We sample from those logits first, then forward each draft token
-- to advance the KV cache.
draftTokens :: InferState -> Int -> Int -> Int -> [Int] -> IO [Int]
draftTokens draftIs _lastToken startPos k eosIds' = go startPos k []
  where
    vs = fromIntegral (lcVocabSize (isConfig draftIs))
    go _pos 0 acc = return (reverse acc)
    go pos remaining acc = do
      -- Sample from draft model's CURRENT logits (already reflect previous token)
      nextTok <- sampleTopP (isLogits draftIs) vs 0.0 1.0 (isRNG draftIs)
      let nt = fromIntegral nextTok
      if nt `elem` eosIds'
        then return (reverse acc)
        else do
          -- Forward draft model with sampled token to advance KV cache
          forward draftIs nt pos
          go (pos + 1) (remaining - 1) (nt : acc)

-- | Downquantize FFN weights from Q8_0 to Q4_0 at runtime.
--
-- For each of the 32 layers, converts gate, up, and down weight matrices
-- from Q8_0 (mmap'd) to Q4_0 (malloc'd). The returned InferState has an
-- overlay map so that tensorPtr returns the Q4 pointers for FFN weights.
--
-- Cost: ~6 seconds for 7B model. Attention weights remain Q8 (zero-copy mmap).
downquantFFNWeights :: InferState -> IO InferState
downquantFFNWeights is = do
  let cfg = isConfig is
      nl  = lcNLayers cfg
      dim = lcDim cfg
      hid = lcHiddenDim cfg
      kbd = dim `div` 32    -- K blocks for gate/up (input dim)
      kbh = hid `div` 32    -- K blocks for down (input dim = hidden)
      -- gate/up: [hidden × kbd] blocks each
      gateUpBlocks = hid * kbd
      -- down: [dim × kbh] blocks
      downBlocks   = dim * kbh
      ffnTensors   = [ ("ffn_gate.weight", gateUpBlocks)
                      , ("ffn_up.weight",   gateUpBlocks)
                      , ("ffn_down.weight", downBlocks)
                      ]

  putStrLn "  Downquantizing FFN weights Q8→Q4..."
  overlay <- foldl (\accIO l -> do
    acc <- accIO
    layerOverlay <- mapM (\(suffix, nBlocks) -> do
      let name = "blk." ++ show l ++ "." ++ suffix
          srcPtr = case Map.lookup name (ggufTensors (isGGUF is)) of
            Just ti -> isBasePtr is `plusPtr`
                       (ggufDataOffset (isGGUF is) + fromIntegral (tiOffset ti))
            Nothing -> error $ "downquantFFNWeights: " ++ name ++ " not found"
      q4Ptr <- downquantFFN srcPtr nBlocks
      return (name, q4Ptr)
      ) ffnTensors
    return (acc ++ layerOverlay)
    ) (return []) [0..nl-1]

  let overlayMap = Map.fromList overlay
      totalQ4Bytes = nl * (gateUpBlocks * 2 + downBlocks) * 18
  putStrLn $ "  Downquantized " ++ show (Map.size overlayMap) ++ " tensors ("
           ++ show (totalQ4Bytes `div` (1024 * 1024)) ++ " MB Q4 allocated)"
  return is { isWeightOverlay = overlayMap }
