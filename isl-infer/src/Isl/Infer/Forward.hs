{-# LANGUAGE RecordWildCards #-}

-- | Full Llama forward pass with fused per-layer kernel.
module Isl.Infer.Forward
  ( InferState(..)
  , initInferState
  , forward
  , generateToken
  ) where

import Data.Int (Int64)
import Data.Word (Word8, Word64)
import qualified Data.Map.Strict as Map
import Foreign.Marshal.Alloc (mallocBytes, callocBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (poke)

import Isl.Infer.GGUF
import Isl.Infer.Model
import Isl.Infer.Compile
import Isl.Infer.Kernel.GEMM (CompiledMatvec(..))
import Isl.Infer.Kernel.FusedLayer (CompiledLayer(..))
import Isl.Infer.Kernel.Elementwise

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
    -- KV cache
  , isKCache   :: !(Ptr Float)
  , isVCache   :: !(Ptr Float)
    -- RoPE precomputed
  , isInvFreq  :: !(Ptr Float)
    -- RNG
  , isRNG      :: !(Ptr Word64)
  }

initInferState :: GGUFFile -> KernelSet -> IO InferState
initInferState gf kernels = do
  let cfg = extractConfig gf
      d   = lcDim cfg
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
  scores  <- mallocBytes (nh * ms * f4)   -- per-head scores for fused kernel
  kCache  <- callocBytes (nl * ms * kvd * f4)
  vCache  <- callocBytes (nl * ms * kvd * f4)

  -- Precompute RoPE inverse frequencies
  let half = headDim `div` 2
      freqBase = lcRopeFreqBase cfg
  invFreq <- mallocBytes (half * f4)
  pokeArray invFreq [ 1.0 / (freqBase ** (2.0 * fromIntegral i / fromIntegral headDim))
                    | i <- [0 .. half - 1] :: [Int] ]

  rng <- mallocBytes 8
  poke rng (42 :: Word64)

  return InferState
    { isConfig = cfg, isGGUF = gf, isBasePtr = basePtr, isKernels = kernels
    , isX = x, isXB = xb, isXB2 = xb2
    , isQ = q, isK = k, isV = v
    , isHB = hb, isHB2 = hb2
    , isLogits = logits, isAttnOut = attnOut, isScores = scores
    , isKCache = kCache, isVCache = vCache
    , isInvFreq = invFreq, isRNG = rng
    }

-- | Raw tensor pointer.
tensorPtr :: InferState -> String -> Ptr Word8
tensorPtr is name =
  case Map.lookup name (ggufTensors (isGGUF is)) of
    Just ti -> isBasePtr is `plusPtr` (ggufDataOffset (isGGUF is) + fromIntegral (tiOffset ti))
    Nothing -> error $ "tensorPtr: " ++ name ++ " not found"

layerW :: InferState -> Int -> String -> Ptr Word8
layerW is l suffix = tensorPtr is ("blk." ++ show l ++ "." ++ suffix)

-- | Forward pass for one token at position pos.
-- Uses the fused per-layer kernel for all transformer layers.
forward :: InferState -> Int -> Int -> IO ()
forward is token pos = do
  let cfg  = isConfig is
      dim  = lcDim cfg
      nl   = lcNLayers cfg
      kvd  = lcKVDim cfg
      ms   = lcMaxSeqLen cfg
      fi   = fromIntegral
      fusedFn = clFn (ksFusedLayer (isKernels is))

  -- 1. Token embedding
  embedToken (isX is) (tensorPtr is "token_embd.weight") (fi token) (fi dim)

  -- 2. Transformer layers via fused kernel
  mapM_ (\l -> do
    let kcLayer = isKCache is `plusPtr` (l * ms * kvd * 4)
        vcLayer = isVCache is `plusPtr` (l * ms * kvd * 4)
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
  callMatvec (ksOutput (isKernels is)) (isLogits is) (isX is) (tensorPtr is "output.weight")

-- | Generate one token.
generateToken :: InferState -> Int -> Int -> Float -> Float -> IO Int
generateToken is token pos temperature topP = do
  forward is token pos
  tok <- sampleTopP (isLogits is) (fromIntegral (lcVocabSize (isConfig is))) temperature topP (isRNG is)
  return (fromIntegral tok)
