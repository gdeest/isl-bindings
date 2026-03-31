{-# LANGUAGE RecordWildCards #-}

-- | Full Llama forward pass with swappable polyhedral kernels.
--
-- The forward pass uses a 'KernelSet' for all GEMM operations.
-- Changing schedules recompiles the KernelSet; the forward pass
-- code doesn't change.
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
import Foreign.Marshal.Array (copyArray, pokeArray)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (poke)

import Isl.Infer.GGUF
import Isl.Infer.Model
import Isl.Infer.Compile
import Isl.Infer.Kernel.GEMM (CompiledMatvec(..))
import Isl.Infer.Kernel.Elementwise

-- | Inference state.
data InferState = InferState
  { isConfig   :: !LlamaConfig
  , isGGUF     :: !GGUFFile
  , isBasePtr  :: !(Ptr Word8)
  , isKernels  :: !KernelSet       -- ^ Swappable! Change schedule → recompile → swap.
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
  , isScores   :: !(Ptr Float)
    -- KV cache
  , isKCache   :: !(Ptr Float)
  , isVCache   :: !(Ptr Float)
    -- RoPE precomputed inverse frequencies [head_dim/2]
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
  scores  <- mallocBytes (ms * f4)
  kCache  <- callocBytes (nl * ms * kvd * f4)
  vCache  <- callocBytes (nl * ms * kvd * f4)
  -- Precompute RoPE inverse frequencies: inv_freq[i] = 1/(freq_base^(2i/head_dim))
  let headDim = lcHeadDim cfg
      freqBase = lcRopeFreqBase cfg
      half = headDim `div` 2
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

-- | Swap kernels (after recompilation with a new schedule).
swapKernels :: InferState -> KernelSet -> InferState
swapKernels is ks = is { isKernels = ks }

-- | Raw tensor pointer.
tensorPtr :: InferState -> String -> Ptr Word8
tensorPtr is name =
  case Map.lookup name (ggufTensors (isGGUF is)) of
    Just ti -> isBasePtr is `plusPtr` (ggufDataOffset (isGGUF is) + fromIntegral (tiOffset ti))
    Nothing -> error $ "tensorPtr: " ++ name ++ " not found"

layerW :: InferState -> Int -> String -> Ptr Word8
layerW is l suffix = tensorPtr is ("blk." ++ show l ++ "." ++ suffix)

-- | Call a specific kernel from the KernelSet.
mv :: InferState -> (KernelSet -> CompiledMatvec)
   -> Ptr Float -> Ptr Float -> Ptr Word8 -> IO ()
mv is sel outPtr xPtr wPtr =
  callMatvec (sel (isKernels is)) outPtr xPtr wPtr

-- | Forward pass for one token at position pos.
forward :: InferState -> Int -> Int -> IO ()
forward is token pos = do
  let cfg  = isConfig is
      dim  = lcDim cfg
      hdim = lcHiddenDim cfg
      nl   = lcNLayers cfg
      nh   = lcNHeads cfg
      nkv  = lcNKVHeads cfg
      hd   = lcHeadDim cfg
      kvd  = lcKVDim cfg
      ms   = lcMaxSeqLen cfg
      eps  = lcRMSNormEps cfg
      freq = lcRopeFreqBase cfg
      x    = isX is
      xb   = isXB is
      xb2  = isXB2 is
      fi   = fromIntegral

  -- Token embedding
  embedToken x (tensorPtr is "token_embd.weight") (fi token) (fi dim)

  -- Transformer layers
  mapM_ (\l -> do
    -- Attention norm
    rmsnorm xb x (castPtr (layerW is l "attn_norm.weight")) (fi dim) eps

    -- QKV via polyhedral kernels
    mv is ksQProj (isQ is) xb (layerW is l "attn_q.weight")
    mv is ksKProj (isK is) xb (layerW is l "attn_k.weight")
    mv is ksVProj (isV is) xb (layerW is l "attn_v.weight")

    -- RoPE
    ropePrecomputed (isQ is) (isK is) (isInvFreq is) (fi nh) (fi nkv) (fi hd) (fi pos)

    -- Update KV cache
    let kcPos = isKCache is `plusPtr` ((l * ms * kvd + pos * kvd) * 4)
        vcPos = isVCache is `plusPtr` ((l * ms * kvd + pos * kvd) * 4)
    copyArray (castPtr kcPos :: Ptr Float) (isK is) kvd
    copyArray (castPtr vcPos :: Ptr Float) (isV is) kvd

    -- Multi-head attention
    let seqLen = pos + 1
        nGroups = nh `div` nkv
    mapM_ (\h -> do
      let kvHead = h `div` nGroups
          qOff   = isQ is `plusPtr` (h * hd * 4)
          outOff = isAttnOut is `plusPtr` (h * hd * 4)
          kcLayer = isKCache is `plusPtr` (l * ms * kvd * 4)
          vcLayer = isVCache is `plusPtr` (l * ms * kvd * 4)
          kcHead  = kcLayer `plusPtr` (kvHead * hd * 4)
          vcHead  = vcLayer `plusPtr` (kvHead * hd * 4)
      attentionHead outOff (castPtr qOff) (castPtr kcHead) (castPtr vcHead)
                     (isScores is) (fi seqLen) (fi hd) (fi kvd)
      ) [0..nh-1]

    -- Output projection
    mv is ksOProj xb2 (isAttnOut is) (layerW is l "attn_output.weight")

    -- Residual
    residualAdd x x xb2 (fi dim)

    -- FFN
    rmsnorm xb x (castPtr (layerW is l "ffn_norm.weight")) (fi dim) eps
    mv is ksGate (isHB is)  xb (layerW is l "ffn_gate.weight")
    mv is ksUp   (isHB2 is) xb (layerW is l "ffn_up.weight")
    siluMul (isHB is) (isHB is) (isHB2 is) (fi hdim)
    mv is ksDown xb2 (isHB is) (layerW is l "ffn_down.weight")
    residualAdd x x xb2 (fi dim)
    ) [0..nl-1]

  -- Final norm + output projection
  rmsnorm x x (castPtr (tensorPtr is "output_norm.weight")) (fi dim) eps
  mv is ksOutput (isLogits is) x (tensorPtr is "output.weight")

-- | Generate one token.
generateToken :: InferState -> Int -> Int -> Float -> Float -> IO Int
generateToken is token pos temperature topP = do
  forward is token pos
  tok <- sampleTopP (isLogits is) (fromIntegral (lcVocabSize (isConfig is))) temperature topP (isRNG is)
  return (fromIntegral tok)
