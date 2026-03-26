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
