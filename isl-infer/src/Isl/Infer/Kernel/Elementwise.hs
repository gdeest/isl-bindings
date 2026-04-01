{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI wrappers for the hand-written C inference kernels.
module Isl.Infer.Kernel.Elementwise
  ( -- * Q8_0 matrix operations
    q8Matvec
  , q8Matmul
    -- * Normalization
  , rmsnorm
    -- * Activations
  , siluMul
    -- * Residual
  , residualAdd
    -- * Position encoding
  , ropePrecomputed
    -- * Attention
  , attentionHead
    -- * Embedding
  , embedToken
    -- * Softmax
  , softmax
    -- * Sampling
  , sampleTopP
    -- * Batched helpers (prefill)
  , rmsnormBatch
  , siluMulBatch
  , residualAddBatch
  , ropeBatch
  , kvCacheWriteBatch
  , attentionPrefillBatch
  ) where

import Data.Int (Int64)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)

-- Q8_0 matrix-vector multiply
foreign import ccall unsafe "q8_matvec"
  q8Matvec :: Ptr Float -> Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> IO ()

-- Q8_0 batched matrix multiply
foreign import ccall unsafe "q8_matmul"
  q8Matmul :: Ptr Float -> Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> Int64 -> IO ()

-- RMSNorm
foreign import ccall unsafe "rmsnorm"
  rmsnorm :: Ptr Float -> Ptr Float -> Ptr Float -> Int64 -> Float -> IO ()

-- SiLU(gate) * up
foreign import ccall unsafe "silu_mul"
  siluMul :: Ptr Float -> Ptr Float -> Ptr Float -> Int64 -> IO ()

-- Residual add
foreign import ccall unsafe "residual_add"
  residualAdd :: Ptr Float -> Ptr Float -> Ptr Float -> Int64 -> IO ()

-- RoPE with precomputed inverse frequencies
foreign import ccall unsafe "rope_precomputed"
  ropePrecomputed :: Ptr Float -> Ptr Float -> Ptr Float -> Int64 -> Int64 -> Int64 -> Int64 -> IO ()

-- Attention head
foreign import ccall unsafe "attention_head"
  attentionHead :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float
                -> Ptr Float -> Int64 -> Int64 -> Int64 -> IO ()

-- Token embedding (Q8_0 dequant)
foreign import ccall unsafe "embed_token"
  embedToken :: Ptr Float -> Ptr Word8 -> Int64 -> Int64 -> IO ()

-- Softmax (in-place)
foreign import ccall unsafe "softmax"
  softmax :: Ptr Float -> Int64 -> IO ()

-- Top-p sampling
foreign import ccall unsafe "sample_top_p"
  sampleTopP :: Ptr Float -> Int64 -> Float -> Float -> Ptr Word64 -> IO Int64

-- ---------------------------------------------------------------------------
-- Batched helpers (prefill) — one FFI call instead of B*NH
-- ---------------------------------------------------------------------------

-- RMSNorm for B tokens
foreign import ccall "rmsnorm_batch"
  rmsnormBatch :: Ptr Float -> Ptr Float -> Ptr Float -> Int64 -> Float -> Int64 -> IO ()

-- SiLU(gate) * up for B tokens
foreign import ccall "silu_mul_batch"
  siluMulBatch :: Ptr Float -> Ptr Float -> Ptr Float -> Int64 -> Int64 -> IO ()

-- Residual add for B tokens
foreign import ccall "residual_add_batch"
  residualAddBatch :: Ptr Float -> Ptr Float -> Ptr Float -> Int64 -> Int64 -> IO ()

-- RoPE for B tokens at consecutive positions
foreign import ccall "rope_batch"
  ropeBatch :: Ptr Float -> Ptr Float -> Ptr Float
            -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
            -> Int64 -> Int64 -> IO ()

-- KV cache write for B tokens (float32 only)
foreign import ccall "kv_cache_write_batch"
  kvCacheWriteBatch :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float
                    -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO ()

-- Batched multi-head causal attention (parallel over heads)
foreign import ccall "attention_prefill_batch"
  attentionPrefillBatch :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Float
                        -> Ptr Float -> Int64 -> Int64 -> Int64 -> Int64
                        -> Int64 -> Int64 -> Int64 -> IO ()
