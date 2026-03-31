{-# LANGUAGE ForeignFunctionInterface #-}

-- | Fused per-layer kernel: all 7 matvecs + attention + elementwise ops
-- in a single C function with ONE persistent OMP parallel region.
--
-- This eliminates the 225 OMP barriers/token bottleneck by reducing
-- to 32 barriers (one per layer), with @nowait@ on independent matvecs.
module Isl.Infer.Kernel.FusedLayer
  ( CompiledLayer(..)
  , compileLayer
  ) where

import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)

import Isl.Infer.Model (LlamaConfig(..))
import Isl.Infer.Runtime

-- | FFI type for the fused layer function.
type LayerFnC
  =  Ptr Float   -- x (in/out, dim)
  -> Ptr Float   -- xb (scratch, dim)
  -> Ptr Float   -- xb2 (scratch, dim)
  -> Ptr Float   -- q (scratch, dim)
  -> Ptr Float   -- k (scratch, kv_dim)
  -> Ptr Float   -- v (scratch, kv_dim)
  -> Ptr Float   -- hb (scratch, hidden)
  -> Ptr Float   -- hb2 (scratch, hidden)
  -> Ptr Float   -- attn_out (scratch, dim)
  -> Ptr Float   -- scores (scratch, max_seq)
  -> Ptr Float   -- k_cache_layer (max_seq * kv_dim)
  -> Ptr Float   -- v_cache_layer (max_seq * kv_dim)
  -> Ptr Word8   -- wq
  -> Ptr Word8   -- wk
  -> Ptr Word8   -- wv
  -> Ptr Word8   -- wo
  -> Ptr Word8   -- w_gate
  -> Ptr Word8   -- w_up
  -> Ptr Word8   -- w_down
  -> Ptr Float   -- attn_norm_weight
  -> Ptr Float   -- ffn_norm_weight
  -> Ptr Float   -- inv_freq
  -> Int64       -- pos
  -> IO ()

foreign import ccall "dynamic"
  mkLayerFn :: FunPtr LayerFnC -> LayerFnC

data CompiledLayer = CompiledLayer
  { clFn     :: !LayerFnC
  , clKernel :: !CompiledKernel
  , clSource :: !String
  }

-- | Generate + compile a fused layer kernel specialized for this model.
compileLayer :: LlamaConfig -> IO CompiledLayer
compileLayer cfg = do
  let src = generateFusedLayerC cfg
  ck <- compileAndLoad "fused_layer" src
  let fn = mkLayerFn (castFunPtr (ckFuncPtr ck))
  return CompiledLayer { clFn = fn, clKernel = ck, clSource = src }

-- | Generate the fused layer C source.
generateFusedLayerC :: LlamaConfig -> String
generateFusedLayerC cfg = unlines $
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , "#include <omp.h>"
  , ""
  , "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
  , ""
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
  , ""
  -- Inline Q8_0 matvec as a macro for reuse
  , "/* Q8_0 matvec: out[0..N-1] = W[N,K] @ x[0..K-1] */"
  , "#define Q8_MATVEC(out, x, W, N, KB) do { \\"
  , "    const block_q8_0* _w = (const block_q8_0*)(W); \\"
  , "    _Pragma(\"omp for nowait schedule(static)\") \\"
  , "    for (int64_t _j = 0; _j < (N); _j++) { \\"
  , "        float _acc = 0.0f; \\"
  , "        for (int64_t _kb = 0; _kb < (KB); _kb++) { \\"
  , "            const block_q8_0* _blk = &_w[_j * (KB) + _kb]; \\"
  , "            float _scale = f16_to_f32(_blk->d); \\"
  , "            float _bsum = 0.0f; \\"
  , "            for (int _v = 0; _v < 32; _v++) \\"
  , "                _bsum += (x)[_kb * 32 + _v] * (float)_blk->qs[_v]; \\"
  , "            _acc += _scale * _bsum; \\"
  , "        } \\"
  , "        (out)[_j] = _acc; \\"
  , "    } \\"
  , "} while(0)"
  , ""
  -- Constants baked in from model config
  , "#define DIM "      ++ show dim
  , "#define KV_DIM "   ++ show kvDim
  , "#define HIDDEN "   ++ show hidden
  , "#define N_HEADS "  ++ show nHeads
  , "#define N_KV "     ++ show nKV
  , "#define HEAD_DIM " ++ show headDim
  , "#define N_GROUPS " ++ show nGroups
  , "#define KB_DIM "   ++ show kbDim
  , "#define KB_HIDDEN " ++ show kbHidden
  , "#define MAX_SEQ "  ++ show maxSeq
  , "#define RMS_EPS "  ++ show eps ++ "f"
  , ""
  , "void fused_layer("
  , "    float* restrict x,"
  , "    float* restrict xb,"
  , "    float* restrict xb2,"
  , "    float* restrict q,"
  , "    float* restrict k,"
  , "    float* restrict v,"
  , "    float* restrict hb,"
  , "    float* restrict hb2,"
  , "    float* restrict attn_out,"
  , "    float* restrict scores,"
  , "    float* restrict k_cache,"
  , "    float* restrict v_cache,"
  , "    const void* restrict wq,"
  , "    const void* restrict wk,"
  , "    const void* restrict wv,"
  , "    const void* restrict wo,"
  , "    const void* restrict w_gate,"
  , "    const void* restrict w_up,"
  , "    const void* restrict w_down,"
  , "    const float* restrict attn_norm_w,"
  , "    const float* restrict ffn_norm_w,"
  , "    const float* restrict inv_freq,"
  , "    int64_t pos"
  , ") {"
  , "    const int64_t seq_len = pos + 1;"
  , ""
  , "    /* --- Attention RMSNorm (serial, fast for dim=4096) --- */"
  , "    {"
  , "        float ss = 0.0f;"
  , "        for (int64_t i = 0; i < DIM; i++) ss += x[i] * x[i];"
  , "        ss = 1.0f / sqrtf(ss / (float)DIM + RMS_EPS);"
  , "        for (int64_t i = 0; i < DIM; i++) xb[i] = x[i] * ss * attn_norm_w[i];"
  , "    }"
  , ""
  , "    /* === SINGLE OMP PARALLEL REGION FOR THE WHOLE LAYER === */"
  , "    #pragma omp parallel"
  , "    {"
  , "        /* --- Q, K, V projections (independent, nowait) --- */"
  , "        Q8_MATVEC(q,  xb, wq, DIM,    KB_DIM);"
  , "        Q8_MATVEC(k,  xb, wk, KV_DIM, KB_DIM);"
  , "        Q8_MATVEC(v,  xb, wv, KV_DIM, KB_DIM);"
  , ""
  , "        /* Barrier: need Q, K, V complete before RoPE + attention */"
  , "        #pragma omp barrier"
  , ""
  , "        /* --- RoPE (single-threaded, fast with precomputed inv_freq) --- */"
  , "        #pragma omp single"
  , "        {"
  , "            for (int64_t h = 0; h < N_HEADS; h++) {"
  , "                float* qh = q + h * HEAD_DIM;"
  , "                for (int64_t i = 0; i < HEAD_DIM / 2; i++) {"
  , "                    float theta = (float)pos * inv_freq[i];"
  , "                    float c = cosf(theta), s = sinf(theta);"
  , "                    float q0 = qh[2*i], q1 = qh[2*i+1];"
  , "                    qh[2*i] = q0*c - q1*s; qh[2*i+1] = q0*s + q1*c;"
  , "                }"
  , "            }"
  , "            for (int64_t h = 0; h < N_KV; h++) {"
  , "                float* kh = k + h * HEAD_DIM;"
  , "                for (int64_t i = 0; i < HEAD_DIM / 2; i++) {"
  , "                    float theta = (float)pos * inv_freq[i];"
  , "                    float c = cosf(theta), s = sinf(theta);"
  , "                    float k0 = kh[2*i], k1 = kh[2*i+1];"
  , "                    kh[2*i] = k0*c - k1*s; kh[2*i+1] = k0*s + k1*c;"
  , "                }"
  , "            }"
  , "            /* KV cache update */"
  , "            memcpy(k_cache + pos * KV_DIM, k, KV_DIM * sizeof(float));"
  , "            memcpy(v_cache + pos * KV_DIM, v, KV_DIM * sizeof(float));"
  , "        } /* end omp single (implicit barrier) */"
  , ""
  , "        /* --- Multi-head attention (parallel over Q-heads) --- */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t h = 0; h < N_HEADS; h++) {"
  , "            int64_t kv_h = h / N_GROUPS;"
  , "            const float* qh = q + h * HEAD_DIM;"
  , "            float* oh = attn_out + h * HEAD_DIM;"
  , "            float* sc = scores + h * MAX_SEQ;"  -- per-head scratch
  , "            float scale = 1.0f / sqrtf((float)HEAD_DIM);"
  , "            /* QK^T */"
  , "            for (int64_t t = 0; t < seq_len; t++) {"
  , "                float dot = 0.0f;"
  , "                const float* kt = k_cache + t * KV_DIM + kv_h * HEAD_DIM;"
  , "                for (int64_t d = 0; d < HEAD_DIM; d++) dot += qh[d] * kt[d];"
  , "                sc[t] = dot * scale;"
  , "            }"
  , "            /* Softmax */"
  , "            float mx = sc[0];"
  , "            for (int64_t t = 1; t < seq_len; t++) if (sc[t] > mx) mx = sc[t];"
  , "            float sm = 0.0f;"
  , "            for (int64_t t = 0; t < seq_len; t++) { sc[t] = expf(sc[t] - mx); sm += sc[t]; }"
  , "            float inv = 1.0f / sm;"
  , "            for (int64_t t = 0; t < seq_len; t++) sc[t] *= inv;"
  , "            /* V accumulation */"
  , "            for (int64_t d = 0; d < HEAD_DIM; d++) oh[d] = 0.0f;"
  , "            for (int64_t t = 0; t < seq_len; t++) {"
  , "                float w = sc[t];"
  , "                const float* vt = v_cache + t * KV_DIM + kv_h * HEAD_DIM;"
  , "                for (int64_t d = 0; d < HEAD_DIM; d++) oh[d] += w * vt[d];"
  , "            }"
  , "        }"
  , "        /* implicit barrier after omp for */"
  , ""
  , "        /* --- O projection (nowait) --- */"
  , "        Q8_MATVEC(xb2, attn_out, wo, DIM, KB_DIM);"
  , ""
  , "        /* --- Residual add (nowait) --- */"
  , "        #pragma omp for nowait"
  , "        for (int64_t i = 0; i < DIM; i++) x[i] += xb2[i];"
  , ""
  , "        #pragma omp barrier"
  , ""
  , "    } /* end first omp parallel */"
  , ""
  , "    /* --- FFN RMSNorm (serial) --- */"
  , "    {"
  , "        float ss = 0.0f;"
  , "        for (int64_t i = 0; i < DIM; i++) ss += x[i] * x[i];"
  , "        ss = 1.0f / sqrtf(ss / (float)DIM + RMS_EPS);"
  , "        for (int64_t i = 0; i < DIM; i++) xb[i] = x[i] * ss * ffn_norm_w[i];"
  , "    }"
  , ""
  , "    /* === SECOND OMP PARALLEL REGION FOR FFN === */"
  , "    #pragma omp parallel"
  , "    {"
  , "        /* --- Gate + Up projections (independent, nowait) --- */"
  , "        Q8_MATVEC(hb,  xb, w_gate, HIDDEN, KB_DIM);"
  , "        Q8_MATVEC(hb2, xb, w_up,   HIDDEN, KB_DIM);"
  , ""
  , "        #pragma omp barrier"
  , ""
  , "        /* --- SiLU(gate) * up --- */"
  , "        #pragma omp for nowait"
  , "        for (int64_t i = 0; i < HIDDEN; i++) {"
  , "            float g = hb[i];"
  , "            hb[i] = g / (1.0f + expf(-g)) * hb2[i];"
  , "        }"
  , ""
  , "        #pragma omp barrier"
  , ""
  , "        /* --- Down projection --- */"
  , "        Q8_MATVEC(xb2, hb, w_down, DIM, KB_HIDDEN);"
  , ""
  , "        #pragma omp barrier"
  , ""
  , "        /* --- Residual add --- */"
  , "        #pragma omp for nowait"
  , "        for (int64_t i = 0; i < DIM; i++) x[i] += xb2[i];"
  , "    } /* end second omp parallel */"
  , "}"
  ]
  where
    dim     = lcDim cfg
    kvDim   = lcKVDim cfg
    hidden  = lcHiddenDim cfg
    nHeads  = lcNHeads cfg
    nKV     = lcNKVHeads cfg
    headDim = lcHeadDim cfg
    nGroups = nHeads `div` nKV
    kbDim   = dim `div` 32
    kbHidden = hidden `div` 32
    maxSeq  = lcMaxSeqLen cfg
    eps     = lcRMSNormEps cfg
