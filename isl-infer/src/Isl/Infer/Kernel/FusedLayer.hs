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
import Isl.Infer.Kernel.GEMM (WeightQuant(..))
import Isl.Infer.Model (LlamaConfig(..))
import Isl.Infer.Runtime
import Isl.Infer.Schedule (KVCacheMode(..), KVQuant(..))
import Isl.Infer.Codegen.Fused (LayerAST(..), projectionAST, attentionAST, emitLayerAST)

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

compileLayer :: KVCacheMode -> WeightQuant -> Bool -> LlamaConfig -> IO CompiledLayer
compileLayer kvMode ffnQ vnni cfg = do
  let src = genLayer kvMode ffnQ vnni cfg
  ck <- compileAndLoad "fused_layer" src
  let fn = mkLayerFn (castFunPtr (ckFuncPtr ck))
  return CompiledLayer { clFn = fn, clKernel = ck, clSource = src }

compileLayerPacked :: KVCacheMode -> WeightQuant -> Bool -> Arch -> LlamaConfig -> IO CompiledLayer
compileLayerPacked kvMode ffnQ vnni arch cfg = do
  let src = genLayerPacked kvMode ffnQ vnni arch cfg
  ck <- compileAndLoad "fused_layer" src
  let fn = mkLayerFn (castFunPtr (ckFuncPtr ck))
  return CompiledLayer { clFn = fn, clKernel = ck, clSource = src }

-- | Extra #define macros for quantized KV cache modes.
kvModeMacros :: KVCacheMode -> LlamaConfig -> [String]
kvModeMacros mode cfg =
  let hd = lcHeadDim cfg
      hdBlocks = hd `div` 32
      q8Stride = hdBlocks * 34
      q4Stride = hdBlocks * 18
      needsBlocks = kvKQuant mode /= QFloat32 || kvVQuant mode /= QFloat32
      needsQ8 = kvKQuant mode == QQ8 || kvVQuant mode == QQ8
      needsQ4 = kvKQuant mode == QQ4 || kvVQuant mode == QQ4
  in (if needsBlocks then [ "#define HD_BLOCKS " ++ show hdBlocks ] else [])
  ++ (if needsQ8 then [ "#define HD_Q8_STRIDE " ++ show q8Stride ] else [])
  ++ (if needsQ4 then [ "#define HD_Q4_STRIDE " ++ show q4Stride ] else [])

-- | Helper C functions for quantized KV cache modes.
kvModeHelpers :: KVCacheMode -> [String]
kvModeHelpers mode
  | kvKQuant mode == QFloat32 && kvVQuant mode == QFloat32 = []
  | otherwise =
  [ "static inline uint16_t f32_to_f16(float f) {"
  , "    return _cvtss_sh(f, 0);"  -- 0 = round to nearest even
  , "}"
  , ""
  , "static inline void quantize_block_q8(block_q8_0*blk, const float*src) {"
  , "    float amax = 0.0f;"
  , "    for (int i=0;i<32;i++){float a=fabsf(src[i]);if(a>amax)amax=a;}"
  , "    float d = amax / 127.0f;"
  , "    blk->d = f32_to_f16(d);"
  , "    float id = (d!=0.0f) ? 1.0f/d : 0.0f;"
  , "    for (int i=0;i<32;i++){"
  , "        int v=(int)roundf(src[i]*id);"
  , "        blk->qs[i]=(int8_t)(v<-128?-128:(v>127?127:v));"
  , "    }"
  , "}"
  ] ++ q4Helpers
  where
    q4Helpers =
      [ ""
      , "typedef struct { uint16_t d; uint8_t qs[16]; } block_q4_0;"
      , ""
      , "static inline void quantize_block_q4(block_q4_0*blk, const float*src) {"
      , "    float amax = 0.0f;"
      , "    for (int i=0;i<32;i++){float a=fabsf(src[i]);if(a>amax)amax=a;}"
      , "    float d = amax / 7.0f;"
      , "    blk->d = f32_to_f16(d);"
      , "    float id = (d!=0.0f) ? 1.0f/d : 0.0f;"
      , "    for (int i=0;i<16;i++){"
      , "        int v0=(int)roundf(src[2*i]*id)+8;"
      , "        int v1=(int)roundf(src[2*i+1]*id)+8;"
      , "        v0=v0<0?0:(v0>15?15:v0);"
      , "        v1=v1<0?0:(v1>15?15:v1);"
      , "        blk->qs[i]=(uint8_t)(v0|(v1<<4));"
      , "    }"
      , "}"
      ]

-- | Generate the RoPE + KV cache write + multi-head attention block.
-- Shared between genLayer and genLayerPacked — the only difference between
-- them is the matvec projection bodies, not the attention block.
genRoPECacheAttention :: KVCacheMode -> LlamaConfig -> String
genRoPECacheAttention kvMode cfg = unlines $
  [ ""
  , "        /* Barrier: need Q, K, V complete before RoPE + attention */"
  , "        #pragma omp barrier"
  , ""
  , "        /* RoPE + KV cache update — serial, fast */"
  , "        #pragma omp single nowait"
  , "        {"
  , "            /* Precompute trig table once — all heads use same values */"
  , "            float rope_cos[HD/2], rope_sin[HD/2];"
  , "            for (int64_t i = 0; i < HD/2; i++) {"
  , "                float th = (float)pos*inv_freq[i];"
  , "                rope_cos[i]=cosf(th); rope_sin[i]=sinf(th);"
  , "            }"
  , "            for (int64_t h = 0; h < NH; h++) {"
  , "                float*qh = q+h*HD;"
  , "                for (int64_t i = 0; i < HD/2; i++) {"
  , "                    float co=rope_cos[i], si=rope_sin[i];"
  , "                    float r0=qh[2*i], r1=qh[2*i+1];"
  , "                    qh[2*i]=r0*co-r1*si; qh[2*i+1]=r0*si+r1*co;"
  , "                }"
  , "            }"
  , "            for (int64_t h = 0; h < NKV; h++) {"
  , "                float*kh = k+h*HD;"
  , "                for (int64_t i = 0; i < HD/2; i++) {"
  , "                    float co=rope_cos[i], si=rope_sin[i];"
  , "                    float r0=kh[2*i], r1=kh[2*i+1];"
  , "                    kh[2*i]=r0*co-r1*si; kh[2*i+1]=r0*si+r1*co;"
  , "                }"
  , "            }"
  ] ++ cacheWriteK (kvKQuant kvMode) ++ cacheWriteV (kvVQuant kvMode) ++
  [ "        }"
  , ""
  , "        /* Barrier: need RoPE done before attention reads q, kc, vc */"
  , "        #pragma omp barrier"
  , ""
  , "        /* Multi-head tiled flash attention (parallel over Q-heads) */"
  , "        #pragma omp for schedule(static)"
  , "        for (int64_t h = 0; h < NH; h++) {"
  , "            int64_t kvh = h/NG;"
  , "            const float*qh = q+h*HD;"
  , "            float*oh = attn_out+h*HD;"
  , "            float tile_sc[ATTN_TILE];"
  , "            float scale = 1.0f/sqrtf((float)HD);"
  , "            float m_prev = -1e30f, l_prev = 0.0f;"
  , "            for (int64_t d=0;d<HD;d++) oh[d]=0;"
  , ""
  , "            for (int64_t t0=0;t0<sl;t0+=ATTN_TILE) {"
  , "                int64_t t1=t0+ATTN_TILE; if(t1>sl)t1=sl;"
  , "                int64_t tlen=t1-t0;"
  , "                float m_tile = -1e30f;"
  ] ++ tiledQKDot (kvKQuant kvMode) ++
  [ "                /* Online softmax correction */"
  , "                float m_new = m_prev>m_tile?m_prev:m_tile;"
  , "                float corr = expf(m_prev-m_new);"
  , "                float l_new = corr*l_prev;"
  , "                for (int64_t d=0;d<HD;d++) oh[d]*=corr;"
  ] ++ tiledVAccum (kvVQuant kvMode) ++
  [ "                m_prev=m_new; l_prev=l_new;"
  , "            }"
  , "            if (l_prev>0.0f) {"
  , "                float iv=1.0f/l_prev;"
  , "                for (int64_t d=0;d<HD;d++) oh[d]*=iv;"
  , "            }"
  , "        }"
  , "        /* implicit barrier from omp for (no nowait) */"
  ]
  where
    -- K cache write (dispatched by K quantization)
    cacheWriteK QFloat32 =
      [ "            for (int64_t h = 0; h < NKV; h++)"
      , "                memcpy(kc + h*MAXS*HD + pos*HD, k + h*HD, HD*sizeof(float));"
      ]
    cacheWriteK QQ8 =
      [ "            for (int64_t h = 0; h < NKV; h++) {"
      , "                block_q8_0* dk = (block_q8_0*)((char*)kc + (h*MAXS + pos)*HD_Q8_STRIDE);"
      , "                for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "                    quantize_block_q8(&dk[b], k + h*HD + b*32);"
      , "            }"
      ]
    cacheWriteK QQ4 =
      [ "            for (int64_t h = 0; h < NKV; h++) {"
      , "                block_q4_0* dk = (block_q4_0*)((char*)kc + (h*MAXS + pos)*HD_Q4_STRIDE);"
      , "                for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "                    quantize_block_q4(&dk[b], k + h*HD + b*32);"
      , "            }"
      ]

    -- V cache write (dispatched by V quantization)
    cacheWriteV QFloat32 =
      [ "            for (int64_t h = 0; h < NKV; h++)"
      , "                memcpy(vc + h*MAXS*HD + pos*HD, v + h*HD, HD*sizeof(float));"
      ]
    cacheWriteV QQ8 =
      [ "            for (int64_t h = 0; h < NKV; h++) {"
      , "                block_q8_0* dv = (block_q8_0*)((char*)vc + (h*MAXS + pos)*HD_Q8_STRIDE);"
      , "                for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "                    quantize_block_q8(&dv[b], v + h*HD + b*32);"
      , "            }"
      ]
    cacheWriteV QQ4 =
      [ "            for (int64_t h = 0; h < NKV; h++) {"
      , "                block_q4_0* dv = (block_q4_0*)((char*)vc + (h*MAXS + pos)*HD_Q4_STRIDE);"
      , "                for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "                    quantize_block_q4(&dv[b], v + h*HD + b*32);"
      , "            }"
      ]

    -- Tiled QK^T: compute tile_sc[0..tlen) and track m_tile
    tiledQKDot QFloat32 =
      [ "                for (int64_t t=0;t<tlen;t++){"
      , "                    float dot=0; const float*kt=kc+kvh*MAXS*HD+(t0+t)*HD;"
      , "                    for (int64_t d=0;d<HD;d++) dot+=qh[d]*kt[d];"
      , "                    tile_sc[t]=dot*scale;"
      , "                    if(tile_sc[t]>m_tile)m_tile=tile_sc[t];"
      , "                }"
      ]
    tiledQKDot QQ8 =
      [ "                for (int64_t t=0;t<tlen;t++){"
      , "                    float dot=0;"
      , "                    const block_q8_0*kt=(const block_q8_0*)((const char*)kc+(kvh*MAXS+t0+t)*HD_Q8_STRIDE);"
      , "                    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                        float s=f16_to_f32(kt[b].d); float bs=0;"
      , "                        for (int vv=0;vv<32;vv++) bs+=qh[b*32+vv]*(float)kt[b].qs[vv];"
      , "                        dot+=s*bs;"
      , "                    }"
      , "                    tile_sc[t]=dot*scale;"
      , "                    if(tile_sc[t]>m_tile)m_tile=tile_sc[t];"
      , "                }"
      ]
    tiledQKDot QQ4 =
      [ "                for (int64_t t=0;t<tlen;t++){"
      , "                    float dot=0;"
      , "                    const block_q4_0*kt=(const block_q4_0*)((const char*)kc+(kvh*MAXS+t0+t)*HD_Q4_STRIDE);"
      , "                    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                        float s=f16_to_f32(kt[b].d); float bs=0;"
      , "                        for (int i=0;i<16;i++){"
      , "                            uint8_t p=kt[b].qs[i];"
      , "                            bs+=qh[b*32+2*i]*(float)((p&0xF)-8)+qh[b*32+2*i+1]*(float)((p>>4)-8);"
      , "                        }"
      , "                        dot+=s*bs;"
      , "                    }"
      , "                    tile_sc[t]=dot*scale;"
      , "                    if(tile_sc[t]>m_tile)m_tile=tile_sc[t];"
      , "                }"
      ]

    -- Tiled V accumulation: add tile contribution with online softmax weights
    tiledVAccum QFloat32 =
      [ "                for (int64_t t=0;t<tlen;t++){"
      , "                    float w=expf(tile_sc[t]-m_new); l_new+=w;"
      , "                    const float*vt=vc+kvh*MAXS*HD+(t0+t)*HD;"
      , "                    for (int64_t d=0;d<HD;d++) oh[d]+=w*vt[d];"
      , "                }"
      ]
    tiledVAccum QQ8 =
      [ "                for (int64_t t=0;t<tlen;t++){"
      , "                    float w=expf(tile_sc[t]-m_new); l_new+=w;"
      , "                    const block_q8_0*vt=(const block_q8_0*)((const char*)vc+(kvh*MAXS+t0+t)*HD_Q8_STRIDE);"
      , "                    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                        float s=f16_to_f32(vt[b].d)*w;"
      , "                        for (int vv=0;vv<32;vv++) oh[b*32+vv]+=s*(float)vt[b].qs[vv];"
      , "                    }"
      , "                }"
      ]
    tiledVAccum QQ4 =
      [ "                for (int64_t t=0;t<tlen;t++){"
      , "                    float w=expf(tile_sc[t]-m_new); l_new+=w;"
      , "                    const block_q4_0*vt=(const block_q4_0*)((const char*)vc+(kvh*MAXS+t0+t)*HD_Q4_STRIDE);"
      , "                    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                        float s=f16_to_f32(vt[b].d)*w;"
      , "                        for (int i=0;i<16;i++){"
      , "                            uint8_t p=vt[b].qs[i];"
      , "                            oh[b*32+2*i]+=s*(float)((p&0xF)-8);"
      , "                            oh[b*32+2*i+1]+=s*(float)((p>>4)-8);"
      , "                        }"
      , "                    }"
      , "                }"
      ]

    -- Legacy QK^T (kept for reference, no longer used by fused layer)
    qkDot QFloat32 =
      [ "            for (int64_t t=0;t<sl;t++){"
      , "                float dot=0; const float*kt=kc+kvh*MAXS*HD+t*HD;"
      , "                for (int64_t d=0;d<HD;d++) dot+=qh[d]*kt[d];"
      , "                sc[t]=dot*scale;"
      , "            }"
      ]
    qkDot QQ8 =
      [ "            for (int64_t t=0;t<sl;t++){"
      , "                float dot=0;"
      , "                const block_q8_0*kt=(const block_q8_0*)((const char*)kc+(kvh*MAXS+t)*HD_Q8_STRIDE);"
      , "                for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                    float s=f16_to_f32(kt[b].d); float bs=0;"
      , "                    for (int vv=0;vv<32;vv++) bs+=qh[b*32+vv]*(float)kt[b].qs[vv];"
      , "                    dot+=s*bs;"
      , "                }"
      , "                sc[t]=dot*scale;"
      , "            }"
      ]
    qkDot QQ4 =
      [ "            for (int64_t t=0;t<sl;t++){"
      , "                float dot=0;"
      , "                const block_q4_0*kt=(const block_q4_0*)((const char*)kc+(kvh*MAXS+t)*HD_Q4_STRIDE);"
      , "                for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                    float s=f16_to_f32(kt[b].d); float bs=0;"
      , "                    for (int i=0;i<16;i++){"
      , "                        uint8_t p=kt[b].qs[i];"
      , "                        bs+=qh[b*32+2*i]*(float)((p&0xF)-8)+qh[b*32+2*i+1]*(float)((p>>4)-8);"
      , "                    }"
      , "                    dot+=s*bs;"
      , "                }"
      , "                sc[t]=dot*scale;"
      , "            }"
      ]

    -- V weighted sum (dispatched by V quantization)
    vAccum QFloat32 =
      [ "            for (int64_t d=0;d<HD;d++)oh[d]=0;"
      , "            for (int64_t t=0;t<sl;t++){"
      , "                float w=sc[t]; const float*vt=vc+kvh*MAXS*HD+t*HD;"
      , "                for (int64_t d=0;d<HD;d++)oh[d]+=w*vt[d];"
      , "            }"
      ]
    vAccum QQ8 =
      [ "            for (int64_t d=0;d<HD;d++)oh[d]=0;"
      , "            for (int64_t t=0;t<sl;t++){"
      , "                float w=sc[t];"
      , "                const block_q8_0*vt=(const block_q8_0*)((const char*)vc+(kvh*MAXS+t)*HD_Q8_STRIDE);"
      , "                for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                    float s=f16_to_f32(vt[b].d)*w;"
      , "                    for (int vv=0;vv<32;vv++) oh[b*32+vv]+=s*(float)vt[b].qs[vv];"
      , "                }"
      , "            }"
      ]
    vAccum QQ4 =
      [ "            for (int64_t d=0;d<HD;d++)oh[d]=0;"
      , "            for (int64_t t=0;t<sl;t++){"
      , "                float w=sc[t];"
      , "                const block_q4_0*vt=(const block_q4_0*)((const char*)vc+(kvh*MAXS+t)*HD_Q4_STRIDE);"
      , "                for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "                    float s=f16_to_f32(vt[b].d)*w;"
      , "                    for (int i=0;i<16;i++){"
      , "                        uint8_t p=vt[b].qs[i];"
      , "                        oh[b*32+2*i]+=s*(float)((p&0xF)-8);"
      , "                        oh[b*32+2*i+1]+=s*(float)((p>>4)-8);"
      , "                    }"
      , "                }"
      , "            }"
      ]

-- | Polyhedral variant of genRoPECacheAttention.
-- RoPE + KV cache: same serial code as original.
-- Attention: polyhedral (h, t0) scanner replaces hand-written loops.
genRoPECacheAttentionPoly :: KVCacheMode -> LlamaConfig -> String
genRoPECacheAttentionPoly kvMode cfg =
  let attnTile = 64 :: Int
      -- RoPE + KV cache (serial, opaque — non-affine trig + quantization)
      ropeCode = unlines $
        [ "/* Precompute trig table once — all heads use same values */"
        , "float rope_cos[HD/2], rope_sin[HD/2];"
        , "for (int64_t i = 0; i < HD/2; i++) {"
        , "    float th = (float)pos*inv_freq[i];"
        , "    rope_cos[i]=cosf(th); rope_sin[i]=sinf(th);"
        , "}"
        , "for (int64_t h = 0; h < NH; h++) {"
        , "    float*qh = q+h*HD;"
        , "    for (int64_t i = 0; i < HD/2; i++) {"
        , "        float co=rope_cos[i], si=rope_sin[i];"
        , "        float r0=qh[2*i], r1=qh[2*i+1];"
        , "        qh[2*i]=r0*co-r1*si; qh[2*i+1]=r0*si+r1*co;"
        , "    }"
        , "}"
        , "for (int64_t h = 0; h < NKV; h++) {"
        , "    float*kh = k+h*HD;"
        , "    for (int64_t i = 0; i < HD/2; i++) {"
        , "        float co=rope_cos[i], si=rope_sin[i];"
        , "        float r0=kh[2*i], r1=kh[2*i+1];"
        , "        kh[2*i]=r0*co-r1*si; kh[2*i+1]=r0*si+r1*co;"
        , "    }"
        , "}"
        ] ++ cacheWriteK' (kvKQuant kvMode) ++ cacheWriteV' (kvVQuant kvMode)

      -- Per-head init: zero output, init softmax state
      headInit = unlines
        [ "int64_t kvh = h/NG;"
        , "const float*qh = q+h*HD;"
        , "float*oh = attn_out+h*HD;"
        , "float tile_sc[" ++ show attnTile ++ "];"
        , "float scale = 1.0f/sqrtf((float)HD);"
        , "float m_prev = -1e30f, l_prev = 0.0f;"
        , "for (int64_t d=0;d<HD;d++) oh[d]=0;"
        ]

      -- Per-tile body: QK^T + online softmax + V accumulation
      tileBody = unlines $
        [ "int64_t t1=t0*" ++ show attnTile ++ "+" ++ show attnTile ++ "; if(t1>sl)t1=sl;"
        , "int64_t tlen=t1-t0*" ++ show attnTile ++ ";"
        , "int64_t t0_off = t0 * " ++ show attnTile ++ ";"
        , "float m_tile = -1e30f;"
        ] ++ tiledQKDot' (kvKQuant kvMode) ++
        [ "/* Online softmax correction */"
        , "float m_new = m_prev>m_tile?m_prev:m_tile;"
        , "float corr = expf(m_prev-m_new);"
        , "float l_new = corr*l_prev;"
        , "for (int64_t d=0;d<HD;d++) oh[d]*=corr;"
        ] ++ tiledVAccum' (kvVQuant kvMode) ++
        [ "m_prev=m_new; l_prev=l_new;" ]

      -- Per-head finalize: normalize by softmax denominator
      headFinalize = unlines
        [ "if (l_prev>0.0f) {"
        , "    float iv=1.0f/l_prev;"
        , "    for (int64_t d=0;d<HD;d++) oh[d]*=iv;"
        , "}"
        ]

      -- Compose via LayerAST
      attnFragment = LSeq
        [ LBarrier
        , LSerial ropeCode
        , LBarrier
        , LComment "Multi-head tiled flash attention — polyhedral (h, t0) scanner"
        , attentionAST attnTile "NH" "sl" headInit tileBody headFinalize
        ]
  in unlines (emitLayerAST 2 attnFragment)  -- depth 2: inside function + omp parallel
  where
    -- KV cache write helpers (indentation adjusted for omp single context)
    cacheWriteK' QFloat32 =
      [ "for (int64_t h = 0; h < NKV; h++)"
      , "    memcpy(kc + h*MAXS*HD + pos*HD, k + h*HD, HD*sizeof(float));"
      ]
    cacheWriteK' QQ8 =
      [ "for (int64_t h = 0; h < NKV; h++) {"
      , "    block_q8_0* dk = (block_q8_0*)((char*)kc + (h*MAXS + pos)*HD_Q8_STRIDE);"
      , "    for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "        quantize_block_q8(&dk[b], k + h*HD + b*32);"
      , "}"
      ]
    cacheWriteK' QQ4 =
      [ "for (int64_t h = 0; h < NKV; h++) {"
      , "    block_q4_0* dk = (block_q4_0*)((char*)kc + (h*MAXS + pos)*HD_Q4_STRIDE);"
      , "    for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "        quantize_block_q4(&dk[b], k + h*HD + b*32);"
      , "}"
      ]
    cacheWriteV' QFloat32 =
      [ "for (int64_t h = 0; h < NKV; h++)"
      , "    memcpy(vc + h*MAXS*HD + pos*HD, v + h*HD, HD*sizeof(float));"
      ]
    cacheWriteV' QQ8 =
      [ "for (int64_t h = 0; h < NKV; h++) {"
      , "    block_q8_0* dv = (block_q8_0*)((char*)vc + (h*MAXS + pos)*HD_Q8_STRIDE);"
      , "    for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "        quantize_block_q8(&dv[b], v + h*HD + b*32);"
      , "}"
      ]
    cacheWriteV' QQ4 =
      [ "for (int64_t h = 0; h < NKV; h++) {"
      , "    block_q4_0* dv = (block_q4_0*)((char*)vc + (h*MAXS + pos)*HD_Q4_STRIDE);"
      , "    for (int64_t b = 0; b < HD_BLOCKS; b++)"
      , "        quantize_block_q4(&dv[b], v + h*HD + b*32);"
      , "}"
      ]
    -- Tiled QK^T (uses t0_off for tile offset instead of raw t0)
    tiledQKDot' QFloat32 =
      [ "for (int64_t t=0;t<tlen;t++){"
      , "    float dot=0; const float*kt=kc+kvh*MAXS*HD+(t0_off+t)*HD;"
      , "    for (int64_t d=0;d<HD;d++) dot+=qh[d]*kt[d];"
      , "    tile_sc[t]=dot*scale;"
      , "    if(tile_sc[t]>m_tile)m_tile=tile_sc[t];"
      , "}"
      ]
    tiledQKDot' QQ8 =
      [ "for (int64_t t=0;t<tlen;t++){"
      , "    float dot=0;"
      , "    const block_q8_0*kt=(const block_q8_0*)((const char*)kc+(kvh*MAXS+t0_off+t)*HD_Q8_STRIDE);"
      , "    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "        float s=f16_to_f32(kt[b].d); float bs=0;"
      , "        for (int vv=0;vv<32;vv++) bs+=qh[b*32+vv]*(float)kt[b].qs[vv];"
      , "        dot+=s*bs;"
      , "    }"
      , "    tile_sc[t]=dot*scale;"
      , "    if(tile_sc[t]>m_tile)m_tile=tile_sc[t];"
      , "}"
      ]
    tiledQKDot' QQ4 =
      [ "for (int64_t t=0;t<tlen;t++){"
      , "    float dot=0;"
      , "    const block_q4_0*kt=(const block_q4_0*)((const char*)kc+(kvh*MAXS+t0_off+t)*HD_Q4_STRIDE);"
      , "    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "        float s=f16_to_f32(kt[b].d); float bs=0;"
      , "        for (int i=0;i<16;i++){"
      , "            uint8_t p=kt[b].qs[i];"
      , "            bs+=qh[b*32+2*i]*(float)((p&0xF)-8)+qh[b*32+2*i+1]*(float)((p>>4)-8);"
      , "        }"
      , "        dot+=s*bs;"
      , "    }"
      , "    tile_sc[t]=dot*scale;"
      , "    if(tile_sc[t]>m_tile)m_tile=tile_sc[t];"
      , "}"
      ]
    -- Tiled V accumulation
    tiledVAccum' QFloat32 =
      [ "for (int64_t t=0;t<tlen;t++){"
      , "    float w=expf(tile_sc[t]-m_new); l_new+=w;"
      , "    const float*vt=vc+kvh*MAXS*HD+(t0_off+t)*HD;"
      , "    for (int64_t d=0;d<HD;d++) oh[d]+=w*vt[d];"
      , "}"
      ]
    tiledVAccum' QQ8 =
      [ "for (int64_t t=0;t<tlen;t++){"
      , "    float w=expf(tile_sc[t]-m_new); l_new+=w;"
      , "    const block_q8_0*vt=(const block_q8_0*)((const char*)vc+(kvh*MAXS+t0_off+t)*HD_Q8_STRIDE);"
      , "    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "        float s=f16_to_f32(vt[b].d)*w;"
      , "        for (int vv=0;vv<32;vv++) oh[b*32+vv]+=s*(float)vt[b].qs[vv];"
      , "    }"
      , "}"
      ]
    tiledVAccum' QQ4 =
      [ "for (int64_t t=0;t<tlen;t++){"
      , "    float w=expf(tile_sc[t]-m_new); l_new+=w;"
      , "    const block_q4_0*vt=(const block_q4_0*)((const char*)vc+(kvh*MAXS+t0_off+t)*HD_Q4_STRIDE);"
      , "    for (int64_t b=0;b<HD_BLOCKS;b++){"
      , "        float s=f16_to_f32(vt[b].d)*w;"
      , "        for (int i=0;i<16;i++){"
      , "            uint8_t p=vt[b].qs[i];"
      , "            oh[b*32+2*i]+=s*(float)((p&0xF)-8);"
      , "            oh[b*32+2*i+1]+=s*(float)((p>>4)-8);"
      , "        }"
      , "    }"
      , "}"
      ]

-- | Generate the fused layer.
-- Two OMP parallel regions per layer (attn + FFN). Within each region,
-- independent matvecs use @omp for nowait@ to eliminate fork-join overhead.
-- Explicit barriers only where data dependencies require them.
--
-- Previous version: 9 separate @omp parallel for@ regions per layer (288/token).
-- This version: 2 @omp parallel@ regions per layer (64/token).
genLayer :: KVCacheMode -> WeightQuant -> Bool -> LlamaConfig -> String
genLayer kvMode ffnQ vnni cfg = unlines $
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , "#include <omp.h>"
  , ""
  , "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
  ] ++ ffnQTypedefs ffnQ ++
  [ "#include <immintrin.h>"
  , "static inline float f16_to_f32(uint16_t h) { return _cvtsh_ss(h); }"
  , ""
  , "/* Inline matvec body for use inside omp parallel regions."
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
  , "#define ATTN_TILE 64"
  ] ++ kvModeMacros kvMode cfg ++
  [ ""
  ] ++ kvModeHelpers kvMode ++
  [ ""
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
  ] ++ ffnWCasts ffnQ ++
  vnniDeclare vnni "xb_q8" "xb_d" "KBD" ++
  vnniDeclare vnni "ao_q8" "ao_d" "KBD" ++
  vnniDeclare vnni "hb_q8" "hb_d" "KBH" ++
  [ ""
  , "    /* ---- ATTENTION BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, anw, DIM);"
  ] ++ vnniQuantize vnni "xb" "xb_q8" "xb_d" "KBD" ++
  [ ""
  , "    #pragma omp parallel"
  , "    {"
  ] ++ scalarQ8Proj vnni "Q" "q"  "xb" "xb_q8" "xb_d" "Wq" "DIM" "KBD" True
    ++ scalarQ8Proj vnni "K" "k"  "xb" "xb_q8" "xb_d" "Wk" "KVD" "KBD" True
    ++ scalarQ8Proj vnni "V" "v"  "xb" "xb_q8" "xb_d" "Wv" "KVD" "KBD" True ++
  [ ""
  , genRoPECacheAttention kvMode cfg
  ] ++ vnniQuantize vnni "attn_out" "ao_q8" "ao_d" "KBD" ++
  scalarQ8Proj vnni "O" "xb2" "attn_out" "ao_q8" "ao_d" "Wo" "DIM" "KBD" False ++
  [ "        /* implicit barrier from omp for */"
  , ""
  , "        /* Residual connection */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t i=0;i<DIM;i++) x[i]+=xb2[i];"
  , "    } /* end attn omp parallel */"
  , ""
  , "    /* ---- FFN BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, fnw, DIM);"
  ] ++ vnniQuantize vnni "xb" "xb_q8" "xb_d" "KBD" ++
  [ ""
  , "    #pragma omp parallel"
  , "    {"
  ] ++ scalarFFNProjection ffnQ vnni "Gate" "hb"  "xb" "xb_q8" "xb_d" "Wg" "HID" "KBD" True
    ++ scalarFFNProjection ffnQ vnni "Up"   "hb2" "xb" "xb_q8" "xb_d" "Wu" "HID" "KBD" True ++
  [ ""
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
  ] ++ vnniQuantize vnni "hb" "hb_q8" "hb_d" "KBH" ++
  scalarFFNProjection ffnQ vnni "Down" "xb2" "hb" "hb_q8" "hb_d" "Wd" "DIM" "KBH" False ++
  [ "        /* implicit barrier from omp for */"
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
genLayerPacked :: KVCacheMode -> WeightQuant -> Bool -> Arch -> LlamaConfig -> String
genLayerPacked kvMode ffnQ vnni arch cfg = unlines $
  [ "#include <stdint.h>"
  , "#include <math.h>"
  , "#include <string.h>"
  , "#include <omp.h>"
  , ""
  , "typedef struct { uint16_t d; int8_t qs[32]; } block_q8_0;"
  ] ++ ffnQTypedefs ffnQ ++
  [ "#include <immintrin.h>"
  , "static inline float f16_to_f32(uint16_t h) { return _cvtsh_ss(h); }"
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
  , "#define ATTN_TILE 64"
  ] ++ kvModeMacros kvMode cfg ++
  [ ""
  ] ++ kvModeHelpers kvMode ++
  [ ""
  , "/* ISL polyhedral codegen macros */"
  , "#define ISL_CEIL_DIV(a, b) (((a) >= 0) ? (((a) + (b) - 1) / (b)) : -(-(a) / (b)))"
  , "#define ISL_FLOOR_DIV(a, b) (((a) >= 0) ? ((a) / (b)) : -((-(a) + (b) - 1) / (b)))"
  , "#define ISL_MAX(a, b) ((a) > (b) ? (a) : (b))"
  , "#define ISL_MIN(a, b) ((a) < (b) ? (a) : (b))"
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
  ] ++ ffnWCasts ffnQ ++
  vnniDeclare vnni "xb_q8" "xb_d" "KBD" ++
  vnniDeclare vnni "ao_q8" "ao_d" "KBD" ++
  vnniDeclare vnni "hb_q8" "hb_d" "KBH" ++
  [ ""
  , "    /* ---- ATTENTION BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, anw, DIM);"
  ] ++ vnniQuantize vnni "xb" "xb_q8" "xb_d" "KBD" ++
  [ ""
  , "    #pragma omp parallel"
  , "    {"
  , packedProjection WQ8 vnni tDim "Q" "q" "xb" "xb_q8" "xb_d" "Wq" "DIM" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , packedProjection WQ8 vnni tDim "K" "k" "xb" "xb_q8" "xb_d" "Wk" "KVD" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , packedProjection WQ8 vnni tDim "V" "v" "xb" "xb_q8" "xb_d" "Wv" "KVD" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , genRoPECacheAttentionPoly kvMode cfg
  ] ++ vnniQuantize vnni "attn_out" "ao_q8" "ao_d" "KBD" ++
  [ ""
  , packedProjection WQ8 vnni tDim "O" "xb2" "attn_out" "ao_q8" "ao_d" "Wo" "DIM" "KBD" "D_TJ" "D_TKB" "D_TK" False
  , "        /* implicit barrier from omp for */"
  , ""
  , "        /* Residual connection */"
  , "        #pragma omp for nowait schedule(static)"
  , "        for (int64_t i=0;i<DIM;i++) x[i]+=xb2[i];"
  , "    } /* end attn omp parallel */"
  , ""
  , "    /* ---- FFN BLOCK ---- */"
  , "    rmsnorm_serial(xb, x, fnw, DIM);"
  ] ++ vnniQuantize vnni "xb" "xb_q8" "xb_d" "KBD" ++
  [ ""
  , "    #pragma omp parallel"
  , "    {"
  , packedProjection ffnQ vnni tDim "Gate" "hb" "xb" "xb_q8" "xb_d" "Wg" "HID" "KBD" "D_TJ" "D_TKB" "D_TK" True
  , packedProjection ffnQ vnni tDim "Up" "hb2" "xb" "xb_q8" "xb_d" "Wu" "HID" "KBD" "D_TJ" "D_TKB" "D_TK" True
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
  ] ++ vnniQuantize vnni "hb" "hb_q8" "hb_d" "KBH" ++
  [ ""
  , packedProjection ffnQ vnni tHid "Down" "xb2" "hb" "hb_q8" "hb_d" "Wd" "DIM" "KBH" "H_TJ" "H_TKB" "H_TK" False
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

-- | Extra typedefs for FFN weight quantization format.
ffnQTypedefs :: WeightQuant -> [String]
ffnQTypedefs WQ4 = ["typedef struct { uint16_t d; uint8_t qs[16]; } block_q4_0;"]
ffnQTypedefs WQ4K =
  [ "typedef struct {"
  , "    uint16_t d; uint16_t dmin;"
  , "    uint8_t scales[12]; uint8_t qs[128];"
  , "} block_q4_K;"
  ]
ffnQTypedefs _ = []

-- | FFN weight pointer casts, used by both genLayer and genLayerPacked.
ffnWCasts :: WeightQuant -> [String]
ffnWCasts WQ8 =
  [ "    const block_q8_0* Wg = (const block_q8_0*)wg;"
  , "    const block_q8_0* Wu = (const block_q8_0*)wu;"
  , "    const block_q8_0* Wd = (const block_q8_0*)wd;"
  ]
ffnWCasts WQ4 =
  [ "    const block_q4_0* Wg = (const block_q4_0*)wg;"
  , "    const block_q4_0* Wu = (const block_q4_0*)wu;"
  , "    const block_q4_0* Wd = (const block_q4_0*)wd;"
  ]
ffnWCasts WQ4K =
  [ "    const block_q4_K* Wg = (const block_q4_K*)wg;"
  , "    const block_q4_K* Wu = (const block_q4_K*)wu;"
  , "    const block_q4_K* Wd = (const block_q4_K*)wd;"
  ]

-- | Declare VNNI quantization arrays at function scope. Emits nothing when vnni=False.
vnniDeclare :: Bool -> String -> String -> String -> [String]
vnniDeclare False _ _ _ = []
vnniDeclare True q8Var dVar kbMacro =
  [ "    int8_t " ++ q8Var ++ "[" ++ kbMacro ++ " * 32];"
  , "    float " ++ dVar ++ "[" ++ kbMacro ++ "];"
  ]

-- | Fill VNNI quantization arrays (re-quantize). Emits nothing when vnni=False.
vnniQuantize :: Bool -> String -> String -> String -> String -> [String]
vnniQuantize False _ _ _ _ = []
vnniQuantize True inpVar q8Var dVar kbMacro =
  [ "    /* Quantize " ++ inpVar ++ " to Q8_0 for VNNI int8x8->int32 */"
  , "    for (int64_t qb = 0; qb < " ++ kbMacro ++ "; qb++) {"
  , "        float amax = 0.0f;"
  , "        for (int v = 0; v < 32; v++) {"
  , "            float av = fabsf(" ++ inpVar ++ "[qb * 32 + v]);"
  , "            if (av > amax) amax = av;"
  , "        }"
  , "        " ++ dVar ++ "[qb] = amax / 127.0f;"
  , "        float id = amax > 0.0f ? 127.0f / amax : 0.0f;"
  , "        for (int v = 0; v < 32; v++)"
  , "            " ++ q8Var ++ "[qb * 32 + v] = (int8_t)roundf(" ++ inpVar ++ "[qb * 32 + v] * id);"
  , "    }"
  ]

-- | Generate a Q8 attention projection (Q/K/V/O) with optional VNNI.
scalarQ8Proj :: Bool -> String -> String -> String -> String -> String
             -> String -> String -> String -> Bool -> [String]
scalarQ8Proj useVnni label outVar inpVar q8Var dVar wVar nMacro kbMacro nowait =
  let nw = if nowait then "nowait " else ""
      innerLoop
        | useVnni =
          [ "                int32_t isum = 0;"
          , "                for (int vv = 0; vv < 32; vv++)"
          , "                    isum += (int32_t)blk->qs[vv] * (int32_t)" ++ q8Var ++ "[kb*32+vv];"
          , "                acc += f16_to_f32(blk->d) * " ++ dVar ++ "[kb] * (float)isum;"
          ]
        | otherwise =
          [ "                float bs = 0.0f;"
          , "                for (int vv = 0; vv < 32; vv++)"
          , "                    bs += " ++ inpVar ++ "[kb*32+vv] * (float)blk->qs[vv];"
          , "                acc += f16_to_f32(blk->d) * bs;"
          ]
  in [ ""
     , "        /* " ++ label ++ " projection [" ++ nMacro ++ " x " ++ kbMacro ++ "*32] — " ++ nw ++ "*/"
     , "        #pragma omp for " ++ nw ++ "schedule(static)"
     , "        for (int64_t j = 0; j < " ++ nMacro ++ "; j++) {"
     , "            float acc = 0.0f;"
     , "            for (int64_t kb = 0; kb < " ++ kbMacro ++ "; kb++) {"
     , "                const block_q8_0* blk = &" ++ wVar ++ "[j * " ++ kbMacro ++ " + kb];"
     ] ++ innerLoop ++
     [ "            }"
     , "            " ++ outVar ++ "[j] = acc;"
     , "        }"
     ]

-- | Generate a scalar (non-packed) FFN projection body with Q8 or Q4 inner loop.
scalarFFNProjection :: WeightQuant -> Bool -> String -> String -> String -> String -> String
                    -> String -> String -> String -> Bool -> [String]
scalarFFNProjection wq useVnni label outVar inpVar q8Var dVar wVar nMacro kbMacro nowait
  -- Q4_K has a different block access pattern (super-blocks of 256)
  | wq == WQ4K = scalarFFNProjectionQ4K useVnni label outVar inpVar q8Var dVar wVar nMacro kbMacro nowait
  | otherwise =
  let nw = if nowait then "nowait " else ""
      blkType = case wq of WQ8 -> "block_q8_0"; _ -> "block_q4_0"
      innerLoop = case (wq, useVnni) of
        (WQ8, True) ->
               [ "                int32_t isum = 0;"
               , "                for (int vv = 0; vv < 32; vv++)"
               , "                    isum += (int32_t)blk->qs[vv] * (int32_t)" ++ q8Var ++ "[kb*32+vv];"
               , "                acc += f16_to_f32(blk->d) * " ++ dVar ++ "[kb] * (float)isum;"
               ]
        (WQ8, False) ->
               [ "                float bs = 0.0f;"
               , "                for (int vv = 0; vv < 32; vv++)"
               , "                    bs += " ++ inpVar ++ "[kb*32+vv] * (float)blk->qs[vv];"
               , "                acc += f16_to_f32(blk->d) * bs;"
               ]
        (_, True) ->
               [ "                int32_t isum = 0;"
               , "                for (int vv = 0; vv < 32; vv++) {"
               , "                    int nibble = (blk->qs[vv/2] >> ((vv&1)*4)) & 0xF;"
               , "                    isum += (int32_t)(int8_t)(nibble - 8) * (int32_t)" ++ q8Var ++ "[kb*32+vv];"
               , "                }"
               , "                acc += f16_to_f32(blk->d) * " ++ dVar ++ "[kb] * (float)isum;"
               ]
        (_, False) ->
               [ "                float bs = 0.0f;"
               , "                for (int vv = 0; vv < 32; vv++) {"
               , "                    int nibble = (blk->qs[vv/2] >> ((vv&1)*4)) & 0xF;"
               , "                    bs += " ++ inpVar ++ "[kb*32+vv] * (float)(nibble - 8);"
               , "                }"
               , "                acc += f16_to_f32(blk->d) * bs;"
               ]
  in [ ""
     , "        /* " ++ label ++ " projection [" ++ nMacro ++ " x " ++ kbMacro ++ "*32] — " ++ nw ++ "*/"
     , "        #pragma omp for " ++ nw ++ "schedule(static)"
     , "        for (int64_t j = 0; j < " ++ nMacro ++ "; j++) {"
     , "            float acc = 0.0f;"
     , "            for (int64_t kb = 0; kb < " ++ kbMacro ++ "; kb++) {"
     , "                const " ++ blkType ++ "* blk = &" ++ wVar ++ "[j * " ++ kbMacro ++ " + kb];"
     ] ++ innerLoop ++
     [ "            }"
     , "            " ++ outVar ++ "[j] = acc;"
     , "        }"
     ]

-- | Q4_K scalar FFN projection: super-block indexing with 6-bit scale unpacking.
scalarFFNProjectionQ4K :: Bool -> String -> String -> String -> String -> String
                       -> String -> String -> String -> Bool -> [String]
scalarFFNProjectionQ4K _useVnni label outVar inpVar _q8Var _dVar wVar nMacro kbMacro nowait =
  let nw = if nowait then "nowait " else ""
  in [ ""
     , "        /* " ++ label ++ " projection Q4_K [" ++ nMacro ++ " x " ++ kbMacro ++ "*32] — " ++ nw ++ "*/"
     , "        #pragma omp for " ++ nw ++ "schedule(static)"
     , "        for (int64_t j = 0; j < " ++ nMacro ++ "; j++) {"
     , "            float acc = 0.0f;"
     , "            for (int64_t kb = 0; kb < " ++ kbMacro ++ "; kb++) {"
     , "                int64_t sb = kb / 8;"
     , "                int64_t si = kb & 7;"
     , "                const block_q4_K* sblk = &" ++ wVar ++ "[j * (" ++ kbMacro ++ " / 8) + sb];"
     , "                uint8_t utmp[2];"
     , "                if (si < 4) {"
     , "                    utmp[0] = sblk->scales[si] & 63;"
     , "                    utmp[1] = sblk->scales[si + 4] & 63;"
     , "                } else {"
     , "                    utmp[0] = (sblk->scales[si+4]&0xF)|((sblk->scales[si-4]>>6)<<4);"
     , "                    utmp[1] = (sblk->scales[si+4]>>4)|((sblk->scales[si]>>6)<<4);"
     , "                }"
     , "                float d_sc = f16_to_f32(sblk->d) * utmp[0];"
     , "                float d_mn = f16_to_f32(sblk->dmin) * utmp[1];"
     , "                int64_t qoff = si * 16;"
     , "                float bs = 0.0f;"
     , "                for (int vv = 0; vv < 32; vv++) {"
     , "                    int nibble = (sblk->qs[qoff + vv/2] >> ((vv&1)*4)) & 0xF;"
     , "                    bs += " ++ inpVar ++ "[kb*32+vv] * (d_sc * (float)nibble - d_mn);"
     , "                }"
     , "                acc += bs;"
     , "            }"
     , "            " ++ outVar ++ "[j] = acc;"
     , "        }"
     ]

-- | Generate a packed matvec projection body for use inside an omp parallel region.
--
-- Parameters: weight quant, vnni flag, label, output var, input var, q8 var, d var,
-- weight var, N macro, KB macro, TJ/TKB/TK macros, and whether to use nowait.
--
-- When vnni=True, S1 transposes int8 values (no float dequant) and S2 uses
-- per-block int32 accumulation with scale application at block boundaries.
packedProjection
  :: WeightQuant -> Bool -> Tiles
  -> String -> String -> String -> String -> String -> String
  -> String -> String -> String -> String -> String
  -> Bool -> String
packedProjection wq useVnni tiles label outVar inpVar q8Var dVar wVar nMacro kbMacro tjM tkbM tkM nowait
  | useVnni   = packedProjectionVnni wq label outVar q8Var dVar wVar nMacro kbMacro tjM tkbM tkM nowait
  | otherwise = packedProjectionPoly wq tiles label outVar inpVar wVar nMacro kbMacro nowait

-- | Polyhedral-path packed projection: loop nests generated by ISL scanner.
packedProjectionPoly :: WeightQuant -> Tiles
                     -> String -> String -> String -> String
                     -> String -> String -> Bool -> String
packedProjectionPoly wq tiles label outVar inpVar wVar nMacro kbMacro nowait =
  let tj  = tileJ tiles
      tkb = tileK tiles `div` 32
      ast = projectionAST label wq tj tkb outVar inpVar wVar nMacro kbMacro nowait
  in unlines (emitLayerAST 2 ast)  -- depth 2: inside function + omp parallel

-- | Float-path packed projection (original S1 dequant + S2 broadcast-FMA).
packedProjectionFloat :: WeightQuant -> String -> String -> String -> String
                      -> String -> String -> String -> String -> String -> Bool -> String
packedProjectionFloat wq label outVar inpVar wVar nMacro kbMacro tjM tkbM tkM nowait =
  let blkType = case wq of WQ8 -> "block_q8_0"; WQ4 -> "block_q4_0"
      s1Dequant = case wq of
        WQ8 -> unlines
          [ "                        for (int vv = 0; vv < 32; vv++)"
          , "                            panel[(base + vv) * " ++ tjM ++ " + dj] = scale * (float)blk->qs[vv];"
          ]
        WQ4 -> unlines
          [ "                        for (int vv = 0; vv < 32; vv++) {"
          , "                            int nibble = (blk->qs[vv/2] >> ((vv&1)*4)) & 0xF;"
          , "                            panel[(base + vv) * " ++ tjM ++ " + dj] = scale * (float)(nibble - 8);"
          , "                        }"
          ]
  in unlines
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
  , "                /* S1: Dequant " ++ show wq ++ " blocks → panel[k*TJ + dj] (j-contiguous) */"
  , "                for (int64_t dj = 0; dj < nj; dj++) {"
  , "                    int64_t j = tj + dj;"
  , "                    for (int64_t kb = tkb; kb < kb_end; kb++) {"
  , "                        const " ++ blkType ++ "* blk = &" ++ wVar ++ "[j * " ++ kbMacro ++ " + kb];"
  , "                        float scale = f16_to_f32(blk->d);"
  , "                        int64_t base = (kb - tkb) * 32;"
  , s1Dequant
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

-- | VNNI-path packed projection: S1 transposes int8 weights (no float dequant),
-- S2 accumulates int32 per-block with scale application at block boundaries.
-- Panel buffer is int8 (4× smaller than float), enabling deeper k-tiles in L1.
packedProjectionVnni :: WeightQuant -> String -> String -> String -> String -> String
                     -> String -> String -> String -> String -> String -> Bool -> String
packedProjectionVnni wq label outVar q8Var dVar wVar nMacro kbMacro tjM tkbM tkM nowait =
  let blkType = case wq of WQ8 -> "block_q8_0"; WQ4 -> "block_q4_0"; WQ4K -> "block_q4_K"
      -- S1: transpose int8 weights into panel_i8 + extract per-block scales
      s1Transpose = case wq of
        WQ8 -> unlines
          [ "                        for (int vv = 0; vv < 32; vv++)"
          , "                            panel_i8[(base + vv) * " ++ tjM ++ " + dj] = blk->qs[vv];"
          ]
        -- Q4_K VNNI packed not yet implemented; fallback handled by packedProjection dispatch
        _ -> unlines
          [ "                        for (int vv = 0; vv < 32; vv++) {"
          , "                            int nibble = (blk->qs[vv/2] >> ((vv&1)*4)) & 0xF;"
          , "                            panel_i8[(base + vv) * " ++ tjM ++ " + dj] = (int8_t)(nibble - 8);"
          , "                        }"
          ]
  in unlines
  [ ""
  , "        /* " ++ label ++ " projection — packed VNNI" ++ nw ++ " */"
  , "        #pragma omp for " ++ nw ++ "schedule(static)"
  , "        for (int64_t tj = 0; tj < " ++ nMacro ++ "; tj += " ++ tjM ++ ") {"
  , "            int64_t nj = " ++ tjM ++ ";"
  , "            if (tj + " ++ tjM ++ " > " ++ nMacro ++ ") nj = " ++ nMacro ++ " - tj;"
  , ""
  , "            float acc[" ++ tjM ++ "] __attribute__((aligned(64)));"
  , "            memset(acc, 0, nj * sizeof(float));"
  , "            /* int8 panel: 4× smaller than float panel → deeper k-tiles in L1 */"
  , "            int8_t panel_i8[" ++ tkM ++ " * " ++ tjM ++ "] __attribute__((aligned(64)));"
  , "            float w_scales[" ++ tkbM ++ " * " ++ tjM ++ "];"
  , ""
  , "            for (int64_t tkb = 0; tkb < " ++ kbMacro ++ "; tkb += " ++ tkbM ++ ") {"
  , "                int64_t kb_end = tkb + " ++ tkbM ++ ";"
  , "                if (kb_end > " ++ kbMacro ++ ") kb_end = " ++ kbMacro ++ ";"
  , "                int64_t nkb = kb_end - tkb;"
  , ""
  , "                /* S1: Transpose int8 weights + extract scales (no float dequant) */"
  , "                for (int64_t dj = 0; dj < nj; dj++) {"
  , "                    int64_t j = tj + dj;"
  , "                    for (int64_t kb = tkb; kb < kb_end; kb++) {"
  , "                        const " ++ blkType ++ "* blk = &" ++ wVar ++ "[j * " ++ kbMacro ++ " + kb];"
  , "                        w_scales[(kb - tkb) * " ++ tjM ++ " + dj] = f16_to_f32(blk->d);"
  , "                        int64_t base = (kb - tkb) * 32;"
  , s1Transpose
  , "                    }"
  , "                }"
  , ""
  , "                /* S2: Per-block VNNI int8×int8→int32, then scale to float */"
  , "                for (int64_t kb_off = 0; kb_off < nkb; kb_off++) {"
  , "                    int64_t base = kb_off * 32;"
  , "                    int32_t block_acc[" ++ tjM ++ "] __attribute__((aligned(64)));"
  , "                    memset(block_acc, 0, nj * sizeof(int32_t));"
  , "                    for (int vv = 0; vv < 32; vv++) {"
  , "                        int8_t xk = " ++ q8Var ++ "[(tkb + kb_off) * 32 + vv];"
  , "                        const int8_t* pw = &panel_i8[(base + vv) * " ++ tjM ++ "];"
  , "                        #pragma omp simd"
  , "                        for (int64_t dj = 0; dj < nj; dj++)"
  , "                            block_acc[dj] += (int32_t)pw[dj] * (int32_t)xk;"
  , "                    }"
  , "                    /* Apply combined weight × activation scales */"
  , "                    float xs = " ++ dVar ++ "[tkb + kb_off];"
  , "                    const float* ws = &w_scales[kb_off * " ++ tjM ++ "];"
  , "                    #pragma omp simd"
  , "                    for (int64_t dj = 0; dj < nj; dj++)"
  , "                        acc[dj] += xs * ws[dj] * (float)block_acc[dj];"
  , "                }"
  , "            }"
  , ""
  , "            memcpy(&" ++ outVar ++ "[tj], acc, nj * sizeof(float));"
  , "        }"
  ]
  where nw = if nowait then "nowait " else ""
