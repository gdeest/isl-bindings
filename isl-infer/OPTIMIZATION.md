# isl-infer optimization guide

## Architecture

isl-infer runs LLM inference through **polyhedrally-generated C kernels**:

```
Parametric polyhedron (Haskell, type-safe)
  → Runtime specialization (concrete N, K values from GGUF)
  → Boulet-Feautrier scanner (instant with concrete params)
  → C code generation (with OpenMP, SIMD pragmas)
  → gcc -O3 -march=native -fopenmp (at model load time)
  → dlopen + FFI (hot path during inference)
```

Schedules are swappable at runtime via `/schedule` commands.

## Current measurements

Hardware: AMD Ryzen AI 9 365 (10C/20T, AVX-512 VNNI, L1d=48KB, L2=1MB/core, L3=24MB)
Model: Mistral 7B v0.3 Q8_0 (dim=4096, hidden=14336, 32 layers, 32/8 Q/KV heads)

| Schedule | tok/s | Notes |
|----------|-------|-------|
| `default` (tile=128, OMP) | 0.77 | Per-kernel parallel region |
| `naive` (no tile, no OMP) | 0.50 | Single-threaded baseline |

Forward pass = ~1.3s/token = 225 Q8_0 matvec calls × ~5.8ms average.

## Bottleneck analysis

### 1. OMP barrier overhead (PRIMARY — ~40% of time)

Each of 225 matvec calls per token creates its own `#pragma omp parallel for` region.
The barrier synchronization at region end costs ~10-20μs × 225 = **2-4ms per token**.
With 10 cores, the parallel speedup is only 1.5× instead of expected 5-8×.

**Fix: Fused per-layer kernel.** Generate ONE C function per transformer layer
containing all 7 matvecs + elementwise ops. Single `#pragma omp parallel` region,
`#pragma omp for nowait` on each parallel loop. Reduces 225 barriers to 32.

This aligns naturally with the polyhedral model — it's multi-statement scheduling,
which the existing `Isl.Scan.Multi` infrastructure already supports (see Jacobi demos).

### 2. Memory bandwidth saturation (SECONDARY)

Q8_0 matvec at M=1 has arithmetic intensity of 0.56 FLOPs/byte — extremely
memory-bound. The weight matrix for a 4096×4096 projection is 17.8MB (Q8_0).
At DDR5 ~50 GB/s, minimum latency is 0.36ms per matvec.

Current: ~5.8ms per matvec → ~16× above memory limit.
After fixing OMP overhead: expect ~1-2ms per matvec → 3-5× above limit.

**Fixes:**
- K-tiling keeps x vector in L1 across multiple output rows
- Prefetching weight blocks ahead of computation
- Eliminating f16→f32 conversion overhead (use _cvtsh_ss intrinsic)

### 3. Attention cache layout (TERTIARY)

KV cache is `[n_layers, max_seq, kv_dim]` where kv_dim=1024 interleaves 8 KV heads.
Attention reads head_dim=128 floats at stride 1024 — wastes 7/8 of each cache line.

**Fix:** Transpose to `[n_layers, n_kv_heads, max_seq, head_dim]`.

### 4. RoPE transcendentals (FIXED)

Was: 2048+ `powf()` calls per token.
Now: Precomputed `inv_freq[]` table, only `sinf/cosf` remain.

## Optimization roadmap

### Phase 1: Fused layer kernel (highest impact)

Generate a single C function per transformer layer:

```c
void layer_fused(
    float* x, float* xb, float* xb2,
    float* q, float* k, float* v,
    float* hb, float* hb2, float* attn_out,
    float* scores, float* kv_cache_k, float* kv_cache_v,
    const void* wq, const void* wk, const void* wv, const void* wo,
    const void* w_gate, const void* w_up, const void* w_down,
    const float* attn_norm, const float* ffn_norm,
    const float* inv_freq,
    int64_t pos, int64_t dim, int64_t kv_dim, int64_t hidden_dim,
    int64_t n_heads, int64_t n_kv_heads, int64_t head_dim,
    int64_t kb_dim, int64_t kb_hidden, float rms_eps
) {
    #pragma omp parallel
    {
        // RMSNorm (parallel reduction + scale)
        // ...
        // Q projection (omp for nowait)
        #pragma omp for nowait schedule(static, 16)
        for (int64_t j = ...) { /* Q8_0 matvec body */ }
        // K projection (omp for nowait)
        #pragma omp for nowait schedule(static, 16)
        for (int64_t j = ...) { /* Q8_0 matvec body */ }
        // V projection (omp for nowait)
        #pragma omp for nowait schedule(static, 16)
        for (int64_t j = ...) { /* Q8_0 matvec body */ }
        #pragma omp barrier  // need Q,K,V complete before attention

        // RoPE (parallel over heads)
        // Attention (parallel over Q-heads)
        #pragma omp for schedule(static)
        for (int64_t h = 0; h < n_heads; h++) { /* per-head attention */ }

        // O projection
        #pragma omp for nowait schedule(static, 16)
        for (int64_t j = ...) { /* Q8_0 matvec body */ }
        // Residual add
        // ... etc for FFN
    }
}
```

One `omp parallel` region per layer → 1 thread-pool activation instead of 7.
`nowait` on independent matvecs → zero sync overhead between Q/K/V.
Explicit barriers only where data dependencies require them.

**Expected: 2-4× speedup → 1.5-3 tok/s**

### Phase 2: Head-parallel attention

Inside the fused kernel, attention runs per-head:

```c
#pragma omp for schedule(static)
for (int64_t h = 0; h < n_heads; h++) {
    int kv_h = h / n_groups;
    // QK^T for this head
    for (int64_t t = 0; t < seq_len; t++) {
        scores[h * max_seq + t] = dot(q + h*hd, k_cache + t*kvd + kv_h*hd, hd) * scale;
    }
    softmax(scores + h*max_seq, seq_len);
    // V accumulation
    for (int64_t d = 0; d < hd; d++) {
        attn_out[h*hd + d] = weighted_sum(scores + h*max_seq, v_cache, ...);
    }
}
```

32 heads across 10 cores = ~3 heads/core. Each head's work is independent.
No false sharing (different output regions per head).

**Expected: near-linear scaling for attention portion**

### Phase 3: Memory bandwidth optimizations

After eliminating OMP overhead, the bottleneck shifts to memory bandwidth.

1. **K-tiling for x-vector reuse**: Tile the K dimension so the x-vector slice
   stays in L1 across multiple output rows. Already supported by the polyhedral
   model (`twoLevelMatvecDomain` with `schTileK`).

2. **KV cache transpose**: `[layers, kv_heads, seq, head_dim]` layout makes
   attention dot products unit-stride.

3. **Hardware prefetching hints**: `__builtin_prefetch` on weight blocks 2-3
   iterations ahead.

**Expected: additional 1.5-2× after phase 1+2**

### Phase 4: AVX-512 VNNI intrinsics

The innermost Q8_0 dot product (32 × int8 × float) can use:
- `_mm512_dpbusd_epi32` for int8×int8 → int32 accumulation (64 ops/cycle)
- Or at minimum ensure GCC auto-vectorizes with `-mavx512vnni`

Current `-march=native` in gcc should enable this. Profile with `perf stat`
to verify VNNI instructions are being used.

## Schedule reference

```haskell
-- In the REPL or via /schedule commands:
defaultSchedule = MatvecSchedule
  { schTileJ = 128, schTileJ2 = 0, schTileK = 0
  , schParallel = True, schSimd = True }

naiveSchedule = MatvecSchedule
  { schTileJ = 0, schTileJ2 = 0, schTileK = 0
  , schParallel = False, schSimd = False }

scheduleLarge = MatvecSchedule
  { schTileJ = 192, schTileJ2 = 0, schTileK = 4
  , schParallel = True, schSimd = True }
```

Custom schedules via Haskell code:
```haskell
mySchedule = MatvecSchedule
  { schName = "custom"
  , schTileJ = 256      -- outer J tile for L2
  , schTileJ2 = 0       -- no second-level J tile (yet)
  , schTileK = 8        -- K-tile: 8 blocks = 256 elements of x in L1
  , schParallel = True   -- OMP on outermost tile
  , schSimd = True       -- #pragma omp simd on innermost
  }
```

The polyhedral pipeline verifies tile correctness (bounds, no out-of-range access)
at kernel compile time via the ISL scanner.
