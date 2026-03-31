-- | Llama model configuration and weight extraction from GGUF.
module Isl.Infer.Model
  ( LlamaConfig(..)
  , extractConfig
  ) where

import Data.Word (Word32)
import Isl.Infer.GGUF

-- | Model configuration extracted from GGUF metadata.
data LlamaConfig = LlamaConfig
  { lcDim          :: !Int      -- ^ embedding dimension
  , lcHiddenDim    :: !Int      -- ^ FFN hidden dimension
  , lcNLayers      :: !Int      -- ^ number of transformer layers
  , lcNHeads       :: !Int      -- ^ number of Q attention heads
  , lcNKVHeads     :: !Int      -- ^ number of KV attention heads (GQA)
  , lcHeadDim      :: !Int      -- ^ dimension per head
  , lcVocabSize    :: !Int      -- ^ vocabulary size
  , lcMaxSeqLen    :: !Int      -- ^ maximum context length
  , lcRopeFreqBase :: !Float    -- ^ RoPE frequency base
  , lcRMSNormEps   :: !Float    -- ^ RMSNorm epsilon
  , lcKVDim        :: !Int      -- ^ total KV dimension (n_kv_heads * head_dim)
  } deriving (Show)

-- | Extract model config from GGUF metadata.
extractConfig :: GGUFFile -> LlamaConfig
extractConfig gf =
  let arch = maybe "llama" id (lookupMetaString gf "general.architecture")
      getU32 k = maybe (error $ "missing " ++ k) fromIntegral (lookupMetaU32 gf (arch ++ "." ++ k))
      getF32 k def = maybe def id (lookupMetaF32 gf (arch ++ "." ++ k))

      dim       = getU32 "embedding_length"
      nHeads    = getU32 "attention.head_count"
      nKVHeads  = getU32 "attention.head_count_kv"
      headDim   = dim `div` nHeads
  in LlamaConfig
    { lcDim          = dim
    , lcHiddenDim    = getU32 "feed_forward_length"
    , lcNLayers      = getU32 "block_count"
    , lcNHeads       = nHeads
    , lcNKVHeads     = nKVHeads
    , lcHeadDim      = headDim
    , lcVocabSize    = getU32 "vocab_size"
    , lcMaxSeqLen    = getU32 "context_length"
    , lcRopeFreqBase = getF32 "rope.freq_base" 10000.0
    , lcRMSNormEps   = getF32 "attention.layer_norm_rms_epsilon" 1.0e-5
    , lcKVDim        = nKVHeads * headDim
    }
