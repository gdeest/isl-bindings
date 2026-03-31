{-# LANGUAGE ForeignFunctionInterface #-}

-- | Fused per-layer kernel that calls polyhedrally-generated matvec kernels.
--
-- Two OMP parallel regions per layer (attn + FFN). Each region calls
-- the compiled polyhedral matvec via function pointers. The matvec
-- kernels use nested OMP (omp for inside the existing parallel region).
--
-- Two variants:
--   * 'genLayer': original scalar Q8 body (32-element dot product per block)
--   * 'genLayerPacked': panel-packed microkernel (dequant to L1 panel + broadcast-FMA)
module Isl.Infer.Kernel.FusedLayer
  ( CompiledLayer(..)
  , compileLayer
  , compileLayerPacked
  ) where

import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, FunPtr, castFunPtr)

import Isl.Infer.Arch
import Isl.Infer.Model (LlamaConfig(..))
import Isl.Infer.Runtime

-- | The fused layer function takes all buffers, weights, and FUNCTION POINTERS
-- to the polyhedral matvec kernels. This way the fused layer is a thin
-- scheduling shell and the actual matvec is polyhedrally generated.
type LayerFnC
  =  Ptr Float -> Ptr Float -> Ptr Float       -- x, xb, xb2
  -> Ptr Float -> Ptr Float -> Ptr Float       -- q, k, v
  -> Ptr Float -> Ptr Float                     -- hb, hb2
  -> Ptr Float -> Ptr Float                     -- attn_out, scores
  -> Ptr Float -> Ptr Float                     -- k_cache, v_cache
  -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8  -- wq, wk, wv, wo
  -> Ptr Word8 -> Ptr Word8 -> Ptr Word8               -- w_gate, w_up, w_down
  -> Ptr Float -> Ptr Float                     -- attn_norm, ffn_norm
  -> Ptr Float                                  -- inv_freq
  -> Int64                                      -- pos
  -> IO ()

foreign import ccall "dynamic"
  mkLayerFn :: FunPtr LayerFnC -> LayerFnC

data CompiledLayer = CompiledLayer
  { clFn     :: !LayerFnC
  , clKernel :: !CompiledKernel
  , clSource :: !String
  }

compileLayer :: LlamaConfig -> IO CompiledLayer
compileLayer cfg = do
  let src = genLayer cfg
  ck <- compileAndLoad "fused_layer" src
  let fn = mkLayerFn (castFunPtr (ckFuncPtr ck))
  return CompiledLayer { clFn = fn, clKernel = ck, clSource = src }

compileLayerPacked :: Arch -> LlamaConfig -> IO CompiledLayer
compileLayerPacked arch cfg = do
  let src = genLayerPacked arch cfg
  ck <- compileAndLoad "fused_layer_packed" src
  let fn = mkLayerFn (castFunPtr (ckFuncPtr ck))
  return CompiledLayer { clFn = fn, clKernel = ck, clSource = src }

-- | Generate the fused layer.
-- Two OMP parallel regions per layer (attn + FFN). Within each region,
-- independent matvecs use @omp for nowait@ to eliminate fork-join overhead.
-- Explicit barriers only where data dependencies require them.
--
-- Previous version: 9 separate @omp parallel for@ regions per layer (288/token).
-- This version: 2 @omp parallel@ regions per layer (64/token).
genLayer :: LlamaConfig -> String
genLayer cfg = unlines
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , "#include <omp.h>"
  , ""
  , "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
  , "static inline float f16_to_f32(uint16_t h) {"
  , "    uint32_t sign = (uint32_t)(h >> 15) << 31;"
  , "    uint32_t exp = (h >> 10) & 0x1F;"
  , "    uint32_t mant = h & 0x3FF;"
  , "    uint32_t f;"
  , "    if (exp == 0) {"
  , "        if (mant == 0) { f = sign; }"
  , "        else { exp = 1; while (!(mant & 0x400)) { mant <<= 1; exp--; }"
  , "               mant &= 0x3FF; f = sign | ((exp + 127 - 15) << 23) | (mant << 13); }"
  , "    } else if (exp == 31) { f = sign | 0x7F800000 | (mant << 13); }"
  , "    else { f = sign | ((exp + 127 - 15) << 23) | (mant << 13); }"
  , "    float r; __builtin_memcpy(&r, &f, 4); return r;"
  , "}"
  , ""
  , "/* Inline Q8_0 matvec body for use inside omp parallel regions."
  , "   Each projection gets its own omp for (with or without nowait). */"
  , ""
  , "#define DIM " ++ show dim
  , "#define KVD " ++ show kvd
  , "#define HID " ++ show hid
  , "#define NH " ++ show nh
  , "#define NKV " ++ show nkv
  , "#define HD " ++ show hd
  , "#define NG " ++ show ng
  , "#define KBD " ++ show kbd
  , "#define KBH " ++ show kbh
  , "#define MAXS " ++ show maxs
  , "#define REPS " ++ show eps ++ "f"
  , ""
  , "static void rmsnorm_serial(float*out, const float*inp, const float*w, int64_t n) {"
  , "    float ss = 0.0f;"
  , "    for (int64_t i = 0; i < n; i++) ss += inp[i]*inp[i];"
  , "    ss = 1.0f / sqrtf(ss/(float)n + REPS);"
  , "    for (int64_t i = 0; i < n; i++) out[i] = inp[i]*ss*w[i];"
  , "}"
  , ""
  , "void fused_layer("
  , "    float*x, float*xb, float*xb2,"
  , "    float*q, float*k, float*v,"
  , "    float*hb, float*hb2,"
  , "    float*attn_out, float*scores,"
  , "    float*kc, float*vc,"
  , "    const void*wq, const void*wk, const void*wv, const void*wo,"
  , "    const void*wg, const void*wu, const void*wd,"
  , "    const float*anw, const float*fnw,"
  , "    const float*inv_freq,"
  , "    int64_t pos"
  , ") {"
  , "    const int64_t sl = pos + 1;"
  , "    const block_q8_0* Wq = (const block_q8_0*)wq;"
  , "    const block_q8_0* Wk = (const block_q8_0*)wk;"
  , "    const block_q8_0* Wv = (const block_q8_0*)wv;"
  , "    const block_q8_0* Wo = (const block_q8_0*)wo;"
  , "    const block_q8_0* Wg = (const block_q8_0*)wg;"
  , "    const block_q8_0* Wu = (const block_q8_0*)wu;"
  , "    const block_q8_0* Wd = (const block_q8_0*)wd;"
  , ""
  , "    /* ---- ATTENTION BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, anw, DIM);"
  , ""
  , "    #pragma omp parallel"
  , "    {"
  , "        /* Q projection [DIM x DIM] — nowait, independent of K/V */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t j = 0; j < DIM; j++) {"
  , "            float acc = 0.0f;"
  , "            for (int64_t kb = 0; kb < KBD; kb++) {"
  , "                const block_q8_0* blk = &Wq[j * KBD + kb];"
  , "                float s = f16_to_f32(blk->d);"
  , "                float bs = 0.0f;"
  , "                for (int vv = 0; vv < 32; vv++)"
  , "                    bs += xb[kb*32+vv] * (float)blk->qs[vv];"
  , "                acc += s * bs;"
  , "            }"
  , "            q[j] = acc;"
  , "        }"
  , ""
  , "        /* K projection [KVD x DIM] — nowait */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t j = 0; j < KVD; j++) {"
  , "            float acc = 0.0f;"
  , "            for (int64_t kb = 0; kb < KBD; kb++) {"
  , "                const block_q8_0* blk = &Wk[j * KBD + kb];"
  , "                float s = f16_to_f32(blk->d);"
  , "                float bs = 0.0f;"
  , "                for (int vv = 0; vv < 32; vv++)"
  , "                    bs += xb[kb*32+vv] * (float)blk->qs[vv];"
  , "                acc += s * bs;"
  , "            }"
  , "            k[j] = acc;"
  , "        }"
  , ""
  , "        /* V projection [KVD x DIM] — nowait */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t j = 0; j < KVD; j++) {"
  , "            float acc = 0.0f;"
  , "            for (int64_t kb = 0; kb < KBD; kb++) {"
  , "                const block_q8_0* blk = &Wv[j * KBD + kb];"
  , "                float s = f16_to_f32(blk->d);"
  , "                float bs = 0.0f;"
  , "                for (int vv = 0; vv < 32; vv++)"
  , "                    bs += xb[kb*32+vv] * (float)blk->qs[vv];"
  , "                acc += s * bs;"
  , "            }"
  , "            v[j] = acc;"
  , "        }"
  , ""
  , "        /* Barrier: need Q, K, V complete before RoPE + attention */"
  , "        #pragma omp barrier"
  , ""
  , "        /* RoPE + KV cache update — serial, fast */"
  , "        #pragma omp single nowait"
  , "        {"
  , "            for (int64_t h = 0; h < NH; h++) {"
  , "                float*qh = q+h*HD;"
  , "                for (int64_t i = 0; i < HD/2; i++) {"
  , "                    float th = (float)pos*inv_freq[i];"
  , "                    float co=cosf(th), si=sinf(th);"
  , "                    float r0=qh[2*i], r1=qh[2*i+1];"
  , "                    qh[2*i]=r0*co-r1*si; qh[2*i+1]=r0*si+r1*co;"
  , "                }"
  , "            }"
  , "            for (int64_t h = 0; h < NKV; h++) {"
  , "                float*kh = k+h*HD;"
  , "                for (int64_t i = 0; i < HD/2; i++) {"
  , "                    float th = (float)pos*inv_freq[i];"
  , "                    float co=cosf(th), si=sinf(th);"
  , "                    float r0=kh[2*i], r1=kh[2*i+1];"
  , "                    kh[2*i]=r0*co-r1*si; kh[2*i+1]=r0*si+r1*co;"
  , "                }"
  , "            }"
  , "            memcpy(kc+pos*KVD, k, KVD*sizeof(float));"
  , "            memcpy(vc+pos*KVD, v, KVD*sizeof(float));"
  , "        }"
  , ""
  , "        /* Barrier: need RoPE done before attention reads q, kc, vc */"
  , "        #pragma omp barrier"
  , ""
  , "        /* Multi-head attention (parallel over Q-heads) */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t h = 0; h < NH; h++) {"
  , "            int64_t kvh = h/NG;"
  , "            const float*qh = q+h*HD;"
  , "            float*oh = attn_out+h*HD;"
  , "            float*sc = scores+h*MAXS;"
  , "            float scale = 1.0f/sqrtf((float)HD);"
  , "            for (int64_t t=0;t<sl;t++){"
  , "                float dot=0; const float*kt=kc+t*KVD+kvh*HD;"
  , "                for (int64_t d=0;d<HD;d++) dot+=qh[d]*kt[d];"
  , "                sc[t]=dot*scale;"
  , "            }"
  , "            float mx=sc[0];"
  , "            for (int64_t t=1;t<sl;t++) if(sc[t]>mx)mx=sc[t];"
  , "            float sm=0;"
  , "            for (int64_t t=0;t<sl;t++){sc[t]=expf(sc[t]-mx);sm+=sc[t];}"
  , "            float iv=1.0f/sm;"
  , "            for (int64_t t=0;t<sl;t++)sc[t]*=iv;"
  , "            for (int64_t d=0;d<HD;d++)oh[d]=0;"
  , "            for (int64_t t=0;t<sl;t++){"
  , "                float w=sc[t]; const float*vt=vc+t*KVD+kvh*HD;"
  , "                for (int64_t d=0;d<HD;d++)oh[d]+=w*vt[d];"
  , "            }"
  , "        }"
  , "        /* implicit barrier from omp for (no nowait) */"
  , ""
  , "        /* O projection [DIM x DIM] */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t j = 0; j < DIM; j++) {"
  , "            float acc = 0.0f;"
  , "            for (int64_t kb = 0; kb < KBD; kb++) {"
  , "                const block_q8_0* blk = &Wo[j * KBD + kb];"
  , "                float s = f16_to_f32(blk->d);"
  , "                float bs = 0.0f;"
  , "                for (int vv = 0; vv < 32; vv++)"
  , "                    bs += attn_out[kb*32+vv] * (float)blk->qs[vv];"
  , "                acc += s * bs;"
  , "            }"
  , "            xb2[j] = acc;"
  , "        }"
  , "        /* implicit barrier from omp for */"
  , ""
  , "        /* Residual connection */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t i=0;i<DIM;i++) x[i]+=xb2[i];"
  , "    } /* end attn omp parallel */"
  , ""
  , "    /* ---- FFN BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, fnw, DIM);"
  , ""
  , "    #pragma omp parallel"
  , "    {"
  , "        /* Gate projection [HID x DIM] — nowait */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t j = 0; j < HID; j++) {"
  , "            float acc = 0.0f;"
  , "            for (int64_t kb = 0; kb < KBD; kb++) {"
  , "                const block_q8_0* blk = &Wg[j * KBD + kb];"
  , "                float s = f16_to_f32(blk->d);"
  , "                float bs = 0.0f;"
  , "                for (int vv = 0; vv < 32; vv++)"
  , "                    bs += xb[kb*32+vv] * (float)blk->qs[vv];"
  , "                acc += s * bs;"
  , "            }"
  , "            hb[j] = acc;"
  , "        }"
  , ""
  , "        /* Up projection [HID x DIM] — nowait */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t j = 0; j < HID; j++) {"
  , "            float acc = 0.0f;"
  , "            for (int64_t kb = 0; kb < KBD; kb++) {"
  , "                const block_q8_0* blk = &Wu[j * KBD + kb];"
  , "                float s = f16_to_f32(blk->d);"
  , "                float bs = 0.0f;"
  , "                for (int vv = 0; vv < 32; vv++)"
  , "                    bs += xb[kb*32+vv] * (float)blk->qs[vv];"
  , "                acc += s * bs;"
  , "            }"
  , "            hb2[j] = acc;"
  , "        }"
  , ""
  , "        /* Barrier: need gate + up done before SiLU */"
  , "        #pragma omp barrier"
  , ""
  , "        /* SiLU(gate) * up */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t i=0;i<HID;i++){"
  , "            float g=hb[i]; hb[i]=g/(1.0f+expf(-g))*hb2[i];"
  , "        }"
  , "        /* implicit barrier from omp for */"
  , ""
  , "        /* Down projection [DIM x HID] */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t j = 0; j < DIM; j++) {"
  , "            float acc = 0.0f;"
  , "            for (int64_t kb = 0; kb < KBH; kb++) {"
  , "                const block_q8_0* blk = &Wd[j * KBH + kb];"
  , "                float s = f16_to_f32(blk->d);"
  , "                float bs = 0.0f;"
  , "                for (int vv = 0; vv < 32; vv++)"
  , "                    bs += hb[kb*32+vv] * (float)blk->qs[vv];"
  , "                acc += s * bs;"
  , "            }"
  , "            xb2[j] = acc;"
  , "        }"
  , "        /* implicit barrier from omp for */"
  , ""
  , "        /* Residual connection */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t i=0;i<DIM;i++) x[i]+=xb2[i];"
  , "    } /* end FFN omp parallel */"
  , "}"
  ]
  where
    dim  = lcDim cfg
    kvd  = lcKVDim cfg
    hid  = lcHiddenDim cfg
    nh   = lcNHeads cfg
    nkv  = lcNKVHeads cfg
    hd   = lcHeadDim cfg
    ng   = nh `div` nkv
    kbd  = dim `div` 32
    kbh  = hid `div` 32
    maxs = lcMaxSeqLen cfg
    eps  = lcRMSNormEps cfg

-- | Generate a panel-packed variant of the fused layer.
--
-- Each matvec projection uses the two-stage approach:
--   S1: dequant Q8 blocks → float panel buffer (L1-resident, j-contiguous)
--   S2: broadcast x[k], FMA into SIMD-width accumulators across j
--
-- This enables AVX-512 (zmm) vectorization on the inner j-loop,
-- versus the original's 32-element scalar reduction per Q8 block.
genLayerPacked :: Arch -> LlamaConfig -> String
genLayerPacked arch cfg = unlines
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , "#include <omp.h>"
  , ""
  , "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
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
  , "    float r; __builtin_memcpy(&r, &f, 4); return r;"
  , "}"
  , ""
  , "#define DIM " ++ show dim
  , "#define KVD " ++ show kvd
  , "#define HID " ++ show hid
  , "#define NH " ++ show nh
  , "#define NKV " ++ show nkv
  , "#define HD " ++ show hd
  , "#define NG " ++ show ng
  , "#define KBD " ++ show kbd
  , "#define KBH " ++ show kbh
  , "#define MAXS " ++ show maxs
  , "#define REPS " ++ show eps ++ "f"
  , ""
  , "/* Arch: " ++ archName arch ++ ", SIMD width: " ++ show (archSimdWidth arch) ++ " */"
  , "/* Tiles (K=dim): tileJ=" ++ show (tileJ tDim) ++ " tileK=" ++ show (tileK tDim)
    ++ " nAccum=" ++ show (nAccum tDim) ++ " */"
  , "/* Tiles (K=hid): tileJ=" ++ show (tileJ tHid) ++ " tileK=" ++ show (tileK tHid)
    ++ " nAccum=" ++ show (nAccum tHid) ++ " */"
  , ""
  , "/* Tile constants for projections with K=dim */"
  , "#define D_TJ " ++ show (tileJ tDim)
  , "#define D_TKB " ++ show (tileK tDim `div` 32)
  , "#define D_TK " ++ show (tileK tDim)
  , ""
  , "/* Tile constants for Down projection (K=hid) */"
  , "#define H_TJ " ++ show (tileJ tHid)
  , "#define H_TKB " ++ show (tileK tHid `div` 32)
  , "#define H_TK " ++ show (tileK tHid)
  , ""
  , "static void rmsnorm_serial(float*out, const float*inp, const float*w, int64_t n) {"
  , "    float ss = 0.0f;"
  , "    for (int64_t i = 0; i < n; i++) ss += inp[i]*inp[i];"
  , "    ss = 1.0f / sqrtf(ss/(float)n + REPS);"
  , "    for (int64_t i = 0; i < n; i++) out[i] = inp[i]*ss*w[i];"
  , "}"
  , ""
  , "void fused_layer("
  , "    float*x, float*xb, float*xb2,"
  , "    float*q, float*k, float*v,"
  , "    float*hb, float*hb2,"
  , "    float*attn_out, float*scores,"
  , "    float*kc, float*vc,"
  , "    const void*wq, const void*wk, const void*wv, const void*wo,"
  , "    const void*wg, const void*wu, const void*wd,"
  , "    const float*anw, const float*fnw,"
  , "    const float*inv_freq,"
  , "    int64_t pos"
  , ") {"
  , "    const int64_t sl = pos + 1;"
  , "    const block_q8_0* Wq = (const block_q8_0*)wq;"
  , "    const block_q8_0* Wk = (const block_q8_0*)wk;"
  , "    const block_q8_0* Wv = (const block_q8_0*)wv;"
  , "    const block_q8_0* Wo = (const block_q8_0*)wo;"
  , "    const block_q8_0* Wg = (const block_q8_0*)wg;"
  , "    const block_q8_0* Wu = (const block_q8_0*)wu;"
  , "    const block_q8_0* Wd = (const block_q8_0*)wd;"
  , ""
  , "    /* ---- ATTENTION BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, anw, DIM);"
  , ""
  , "    #pragma omp parallel"
  , "    {"
  , packedProjection "Q" "q" "xb" "Wq" "DIM" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , packedProjection "K" "k" "xb" "Wk" "KVD" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , packedProjection "V" "v" "xb" "Wv" "KVD" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , ""
  , "        /* Barrier: need Q, K, V complete before RoPE + attention */"
  , "        #pragma omp barrier"
  , ""
  , "        /* RoPE + KV cache update — serial, fast */"
  , "        #pragma omp single nowait"
  , "        {"
  , "            for (int64_t h = 0; h < NH; h++) {"
  , "                float*qh = q+h*HD;"
  , "                for (int64_t i = 0; i < HD/2; i++) {"
  , "                    float th = (float)pos*inv_freq[i];"
  , "                    float co=cosf(th), si=sinf(th);"
  , "                    float r0=qh[2*i], r1=qh[2*i+1];"
  , "                    qh[2*i]=r0*co-r1*si; qh[2*i+1]=r0*si+r1*co;"
  , "                }"
  , "            }"
  , "            for (int64_t h = 0; h < NKV; h++) {"
  , "                float*kh = k+h*HD;"
  , "                for (int64_t i = 0; i < HD/2; i++) {"
  , "                    float th = (float)pos*inv_freq[i];"
  , "                    float co=cosf(th), si=sinf(th);"
  , "                    float r0=kh[2*i], r1=kh[2*i+1];"
  , "                    kh[2*i]=r0*co-r1*si; kh[2*i+1]=r0*si+r1*co;"
  , "                }"
  , "            }"
  , "            memcpy(kc+pos*KVD, k, KVD*sizeof(float));"
  , "            memcpy(vc+pos*KVD, v, KVD*sizeof(float));"
  , "        }"
  , ""
  , "        /* Barrier: need RoPE done before attention reads q, kc, vc */"
  , "        #pragma omp barrier"
  , ""
  , "        /* Multi-head attention (parallel over Q-heads) */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t h = 0; h < NH; h++) {"
  , "            int64_t kvh = h/NG;"
  , "            const float*qh = q+h*HD;"
  , "            float*oh = attn_out+h*HD;"
  , "            float*sc = scores+h*MAXS;"
  , "            float scale = 1.0f/sqrtf((float)HD);"
  , "            for (int64_t t=0;t<sl;t++){"
  , "                float dot=0; const float*kt=kc+t*KVD+kvh*HD;"
  , "                for (int64_t d=0;d<HD;d++) dot+=qh[d]*kt[d];"
  , "                sc[t]=dot*scale;"
  , "            }"
  , "            float mx=sc[0];"
  , "            for (int64_t t=1;t<sl;t++) if(sc[t]>mx)mx=sc[t];"
  , "            float sm=0;"
  , "            for (int64_t t=0;t<sl;t++){sc[t]=expf(sc[t]-mx);sm+=sc[t];}"
  , "            float iv=1.0f/sm;"
  , "            for (int64_t t=0;t<sl;t++)sc[t]*=iv;"
  , "            for (int64_t d=0;d<HD;d++)oh[d]=0;"
  , "            for (int64_t t=0;t<sl;t++){"
  , "                float w=sc[t]; const float*vt=vc+t*KVD+kvh*HD;"
  , "                for (int64_t d=0;d<HD;d++)oh[d]+=w*vt[d];"
  , "            }"
  , "        }"
  , "        /* implicit barrier from omp for (no nowait) */"
  , ""
  , packedProjection "O" "xb2" "attn_out" "Wo" "DIM" "KBD" "D_TJ" "D_TKB" "D_TK" False
  , "        /* implicit barrier from omp for */"
  , ""
  , "        /* Residual connection */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t i=0;i<DIM;i++) x[i]+=xb2[i];"
  , "    } /* end attn omp parallel */"
  , ""
  , "    /* ---- FFN BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, fnw, DIM);"
  , ""
  , "    #pragma omp parallel"
  , "    {"
  , packedProjection "Gate" "hb" "xb" "Wg" "HID" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , packedProjection "Up" "hb2" "xb" "Wu" "HID" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , ""
  , "        /* Barrier: need gate + up done before SiLU */"
  , "        #pragma omp barrier"
  , ""
  , "        /* SiLU(gate) * up */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t i=0;i<HID;i++){"
  , "            float g=hb[i]; hb[i]=g/(1.0f+expf(-g))*hb2[i];"
  , "        }"
  , "        /* implicit barrier from omp for */"
  , ""
  , packedProjection "Down" "xb2" "hb" "Wd" "DIM" "KBH" "H_TJ" "H_TKB" "H_TK" False
  , "        /* implicit barrier from omp for */"
  , ""
  , "        /* Residual connection */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t i=0;i<DIM;i++) x[i]+=xb2[i];"
  , "    } /* end FFN omp parallel */"
  , "}"
  ]
  where
    dim  = lcDim cfg
    kvd  = lcKVDim cfg
    hid  = lcHiddenDim cfg
    nh   = lcNHeads cfg
    nkv  = lcNKVHeads cfg
    hd   = lcHeadDim cfg
    ng   = nh `div` nkv
    kbd  = dim `div` 32
    kbh  = hid `div` 32
    maxs = lcMaxSeqLen cfg
    eps  = lcRMSNormEps cfg
    -- Tile sizes for projections with K=dim vs K=hid
    tDim = archTiles arch dim dim
    tHid = archTiles arch dim hid

-- | Generate a packed matvec projection body for use inside an omp parallel region.
--
-- Parameters: label, output var, input var, weight var, N macro, KB macro,
-- TJ/TKB/TK macros, and whether to use nowait.
packedProjection
  :: String -> String -> String -> String
  -> String -> String -> String -> String -> String
  -> Bool -> String
packedProjection label outVar inpVar wVar nMacro kbMacro tjM tkbM tkM nowait = unlines
  [ ""
  , "        /* " ++ label ++ " projection — packed" ++ nw ++ " */"
  , "        #pragma omp for " ++ nw ++ "schedule(static)"
  , "        for (int64_t tj = 0; tj < " ++ nMacro ++ "; tj += " ++ tjM ++ ") {"
  , "            int64_t nj = " ++ tjM ++ ";"
  , "            if (tj + " ++ tjM ++ " > " ++ nMacro ++ ") nj = " ++ nMacro ++ " - tj;"
  , ""
  , "            float acc[" ++ tjM ++ "] __attribute__((aligned(64)));"
  , "            memset(acc, 0, nj * sizeof(float));"
  , "            float panel[" ++ tkM ++ " * " ++ tjM ++ "] __attribute__((aligned(64)));"
  , ""
  , "            for (int64_t tkb = 0; tkb < " ++ kbMacro ++ "; tkb += " ++ tkbM ++ ") {"
  , "                int64_t kb_end = tkb + " ++ tkbM ++ ";"
  , "                if (kb_end > " ++ kbMacro ++ ") kb_end = " ++ kbMacro ++ ";"
  , "                int64_t nkb = kb_end - tkb;"
  , "                int64_t nk = nkb * 32;"
  , ""
  , "                /* S1: Dequant Q8 blocks → panel[k*TJ + dj] (j-contiguous) */"
  , "                for (int64_t dj = 0; dj < nj; dj++) {"
  , "                    int64_t j = tj + dj;"
  , "                    for (int64_t kb = tkb; kb < kb_end; kb++) {"
  , "                        const block_q8_0* blk = &" ++ wVar ++ "[j * " ++ kbMacro ++ " + kb];"
  , "                        float scale = f16_to_f32(blk->d);"
  , "                        int64_t base = (kb - tkb) * 32;"
  , "                        for (int vv = 0; vv < 32; vv++)"
  , "                            panel[(base + vv) * " ++ tjM ++ " + dj] = scale * (float)blk->qs[vv];"
  , "                    }"
  , "                }"
  , ""
  , "                /* S2: Broadcast-FMA — SIMD across j (zmm) */"
  , "                for (int64_t kk = 0; kk < nk; kk++) {"
  , "                    float xk = " ++ inpVar ++ "[(tkb * 32) + kk];"
  , "                    const float* pw = &panel[kk * " ++ tjM ++ "];"
  , "                    #pragma omp simd"
  , "                    for (int64_t dj = 0; dj < nj; dj++)"
  , "                        acc[dj] += xk * pw[dj];"
  , "                }"
  , "            }"
  , ""
  , "            memcpy(&" ++ outVar ++ "[tj], acc, nj * sizeof(float));"
  , "        }"
  ]
  where nw = if nowait then "nowait " else ""
