# ISL Bindings for Haskell

Haskell bindings for the [Integer Set Library](https://libisl.sourceforge.io/) (ISL), with a type-level polyhedral DSL verified by a GHC plugin and a typed Alpha DSL for systems of affine recurrence equations.

## Packages

| Package | Description |
|---------|-------------|
| **bindings** (`isl-bindings`) | FFI bindings to ISL (auto-generated + manual wrappers), linear-typed `IslT` monad for safe memory management |
| **typelevel** (`isl-typelevel`) | Type-level polyhedral DSL: `TExpr`, `TConstraint`, singleton-based evaluation (`SBasicSet`, `SBasicMap`) |
| **plugin** (`isl-plugin`) | GHC typechecker plugin for compile-time polyhedral proof obligations (`IslSubset`, `IslEqual`, …) and type family rewriting (`IslApply`, `IslComplementSet`, …) |
| **alpha** (`isl-alpha`) | Typed DSL for systems of affine recurrence equations (Alpha), with interpreter, schedule validation, and lowering |
| **codegen** (`isl-codegen`) | Code generator: ISL C headers → Haskell FFI binding modules |

### Dependency DAG

```
codegen (standalone)
bindings → typelevel → plugin → alpha
```

## Building

Requires [Nix](https://nixos.org/) for the ISL C library and GHC 9.10+.

```bash
nix develop --command cabal build all
nix develop --command cabal test all
```

## License

MIT
