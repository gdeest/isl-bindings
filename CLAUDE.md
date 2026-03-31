# ISL Bindings for Haskell

Haskell bindings for the Integer Set Library (ISL), with a type-level polyhedral DSL verified by a GHC plugin.

## Project structure

```
bindings/     Low-level FFI bindings (AutoGen + manual Foreach.hs)
highlevel/    Typed wrappers: Set ps n, Map ps ni no (linear types, phantom-indexed)
typelevel/    Type-level DSL: TExpr, TConstraint, singletons, SBasicSet/SBasicMap
plugin/       GHC typechecker plugin: proof obligations + type family rewriting
scan/         Polyhedral scanning (code generation from schedules)
codegen/      ISL header → Haskell FFI code generator
isl-test/     Tests and demos (58 tests, TypeLevelDemo, Jacobi examples)
```

## Building

```
nix develop --command cabal build all
nix develop --command cabal test isl-test
```

Requires nix for the ISL C library dependency.

## Type-level DSL overview

### Defining sets and maps at the type level

```haskell
type Triangle = '[ 'TDim (D 0) >=. 'TConst ('Pos 0)
                 , 'TParam (P "N") >=. 'TDim (D 0)
                 , 'TDim (D 1) >=. 'TConst ('Pos 0)
                 , 'TDim (D 0) >=. 'TDim (D 1) ]

type ShiftRight = '[ 'TDim (D 2) ==. ('TDim (D 0) +. 'TConst ('Pos 1))
                   , 'TDim (D 3) ==. 'TDim (D 1) ]
```

Map constraints use flat (ni+no) indexing: dims 0..ni-1 are input, ni..ni+no-1 are output.

### Plugin proof obligations (compile-time assertions)

```haskell
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
proof :: IslSubset '["N"] 2 Triangle Rectangle => ()
proof = ()  -- compiles iff Triangle ⊆ Rectangle
```

Available: `IslSubset`, `IslNonEmpty`, `IslEqual`, `IslMapSubset`, `IslMapEqual`, `IslRangeOf`, `IslImageSubset`.

### Plugin type family rewriting (compile-time computation)

```haskell
type Complement = IslComplementSet '["N"] 2 Triangle    -- computed by ISL
type Image = IslApply '["N"] 2 2 ShiftRight Rectangle   -- image under map
type Projected = IslProjectOut '["N"] 2 1 1 1 Triangle  -- existential quantification
```

Available: `IslIntersectSet`, `IslComplementSet`, `IslDifferenceSet`, `IslApply`, `IslDomainTF`, `IslRangeTF`, `IslCompose`, `IslReverseMap`, `IslProjectOut`, `IslFromString`, `IslToString`, `IslMapToString`.

### Displaying results in REPL

`:kind!` does NOT trigger the plugin rewriter. Use `symbolVal` instead:

```haskell
ghci> symbolVal (Proxy @(IslToString '["N"] 2 Triangle))
"[N] -> { [i0, i1] : 0 <= i0 <= N and 0 <= i1 <= i0 }"
```

### Singleton-based evaluation (type-level → ISL objects)

```haskell
triangle :: SBasicSet '["N"] 2 Triangle
triangle = sBasicSet  -- auto-derives singleton from type-level info

main = runIslT $ do
  s <- evalSBasicSet triangle  -- ISL Set object
  ...
```

## Conventions

- Parameters (`ps :: [Symbol]`) must be alphabetically sorted (ISL convention)
- `ParamIndex` instances map param names to positional indices (0-based, alphabetical)
- ISL strings are fine for user-facing display (`IslToString`, `IslFromString`) but never for testing/comparison — use ISL predicates (`isEqual`, `isSubset`) instead
- The `FloorDiv`/`TFloorDiv` constructor represents ISL's existential/div dimensions: `floor(expr / d)`

## Vectorization verification

Generated C kernels (in `isl-infer/`) compile with `-O3 -march=native -fopenmp -mavx512f -mavx512bw -mavx512vnni -mf16c`. Always verify GCC actually vectorized inner loops — `#pragma omp simd` is a hint, not a guarantee.

### GCC diagnostic flags

```bash
# Add to gcc invocation for vectorization reports:
gcc ... -fopt-info-vec-optimized -fopt-info-vec-missed ...

# Good: "loop vectorized using 64 byte vectors" (64 bytes = zmm = AVX-512)
# Bad:  "not vectorized: unsupported data-ref" / "complicated access pattern"
```

### Assembly inspection

```bash
objdump -d /tmp/isl_*.so | grep -c 'zmm'     # >> 0 means AVX-512 used
objdump -d /tmp/isl_*.so | grep 'vpdpbusd'   # VNNI int8 dot product (ideal for Q8)
objdump -d /tmp/isl_*.so | grep 'vfmadd'     # FMA instructions
```

Key registers/instructions (Zen 5):
- `zmm0-31`: AVX-512 512-bit — good
- `ymm0-15`: AVX-256 — acceptable (half throughput)
- Only `xmm`/scalar: vectorization failed — bad
- `vpdpbusd`: VNNI int8×int8→int32 — ideal for Q8_0
- `vpmovsxbd` + `vcvtdq2ps`: int8→float widening — means VNNI not used

### Common vectorization blockers

1. `(float)blk->qs[v]` widens int8→float before multiply, preventing VNNI
2. Missing `restrict` on pointer params — aliasing analysis fails
3. `block_q8_0` is 34 bytes (not power-of-2) — can't vectorize across struct boundaries
4. Missing `reduction(+:var)` — GCC sees loop-carried dependency

### Benchmarking

```bash
taskset -c 0-9 cabal run gemm-bench    # pin to cores
```

Roofline (Ryzen AI 9 365): DDR5 ≈ 50 GB/s practical, FP32 FMA ≈ 128 GFLOPS/core. Q8 matvec (M=1) at AI ≈ 2 FLOPs/byte is memory-bound.
