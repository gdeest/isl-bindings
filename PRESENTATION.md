# isl-bindings: A Complete Repository Presentation

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Package-by-Package Walkthrough](#package-by-package-walkthrough)
4. [Design Decisions](#design-decisions)
5. [Concrete Examples](#concrete-examples)
6. [Weird Things](#weird-things)
7. [Build System](#build-system)

---

## Overview

**ISL** (Integer Set Library) is a C library for manipulating sets and relations of integer points bounded by affine constraints. It is a foundational tool in polyhedral compilation — used for static analysis, automatic parallelization, loop tiling, cache optimization, and hardware generation (e.g., by LLVM/Polly, GCC/Graphite, and PPCG).

**isl-bindings** is a Haskell project that wraps ISL in multiple abstraction layers:

| Layer | Package | Purpose |
|-------|---------|---------|
| FFI | `isl-bindings` | Raw C bindings with linear-type memory safety |
| DSL | `isl-bindings-hl` | High-level, type-indexed polyhedral API |
| Scanning | `isl-scan` | Pure Haskell loop nest generation (Boulet-Feautrier) |
| Type-level | `isl-typelevel` | Polyhedral constraints as GHC types |
| Plugin | `isl-plugin` | GHC typechecker plugin — proves polyhedral properties at compile time |
| Codegen | `isl-codegen` | Auto-generates Haskell FFI from ISL C headers |
| Tests | `isl-test` | Examples (matmul, tiling, prefix sum) and property-based tests |

**Status**: Experimental, unsupported.
**GHC**: Requires 9.10+ (for `LinearTypes`, `QualifiedDo`, and the plugin API).
**ISL**: 0.27 (custom-built; nixpkgs ships 0.20 for GCC).

---

## Architecture

### Dependency Graph

```
                     ┌──────────────┐
                     │  isl-codegen │  (standalone executable)
                     └──────┬───────┘
                            │ generates
                            ▼
                     ┌──────────────┐
                     │ isl-bindings │  (FFI layer)
                     └──────┬───────┘
                            │
                     ┌──────▼───────┐
                     │isl-bindings-hl│  (high-level DSL)
                     └──┬───┬───┬───┘
                        │   │   │
              ┌─────────┘   │   └─────────┐
              ▼             ▼             ▼
        ┌──────────┐ ┌────────────┐ ┌──────────┐
        │ isl-scan │ │isl-typelevel│ │isl-plugin│
        └────┬─────┘ └─────┬──────┘ └────┬─────┘
             │              │             │
             └──────────┬───┘─────────────┘
                        ▼
                  ┌──────────┐
                  │ isl-test │  (examples + test suite)
                  └──────────┘
```

### Package Sizes

| Package | Hand-written LoC | Auto-generated LoC |
|---------|------------------|--------------------|
| `isl-bindings` | ~650 | ~10,900 |
| `isl-bindings-hl` | ~1,200 | — |
| `isl-scan` | ~700 | — |
| `isl-typelevel` | ~340 | — |
| `isl-plugin` | ~380 | — |
| `isl-test` | ~570 | — |
| `codegen` | ~500 | — |

---

## Package-by-Package Walkthrough

### 1. `isl-bindings` — FFI Layer

**Key idea**: Every ISL C object has two Haskell representations — an *owned* linear type and a *borrowed* unrestricted reference.

#### Owned vs. Borrowed Types (`bindings/src/Isl/Types.hsc`)

```haskell
-- Owned types (linear — consumed by isl_take functions)
newtype Ctx = Ctx { unCtx :: Ptr Ctx }
newtype BasicSet = BasicSet { unBasicSet :: Ptr BasicSet }
newtype Set = Set { unSet :: Ptr Set }
newtype Map = Map { unMap :: Ptr Map }
-- ... 10 more

-- Borrowed reference types (unrestricted — used by isl_keep functions)
newtype BasicSetRef = BasicSetRef (Ptr BasicSet)
newtype SetRef = SetRef (Ptr Set)
newtype MapRef = MapRef (Ptr Map)
-- ... 10 more
```

This mirrors ISL's C ownership convention:
- `__isl_take` → consumes the pointer (caller loses ownership)
- `__isl_keep` → borrows the pointer (caller retains ownership)

#### Linear Resource Management Typeclasses (`bindings/src/Isl/Types.hsc:70-77`)

```haskell
class Borrow owned ref | owned -> ref where
  borrow :: owned %1 -> (ref -> a) -> (a, owned)

class Consumable a where
  consume :: a %1 -> ()

class Consumable a => Dupable a where
  dup :: a %1 -> (a, a)
```

These are implemented via `unsafeCoerce` + `unsafePerformIO` in `Isl.Instances`:

```haskell
-- bindings/src/Isl/Instances.hs:60-66
instance Consumable BasicSet where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_basic_set_free x)
instance Dupable BasicSet where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_basic_set_copy x; return (x, copy)
instance Borrow BasicSet BasicSetRef where
  borrow = unsafeCoerce $ \(BasicSet ptr) f -> let !r = f (BasicSetRef ptr) in (r, BasicSet ptr)
```

This pattern repeats for all 12 ISL types (Aff, Val, Id, BasicSet, Set, UnionSet, BasicMap, Map, UnionMap, Constraint, Space, LocalSpace).

#### The IslT Monad (`bindings/src/Isl/Monad.hs`)

```haskell
newtype IslT m a = IslT { unIslT :: Ctx -> m a }
type Isl = IslT IO

data Ur a where
  Ur :: a -> Ur a    -- "Unrestricted" — escapes linear tracking

runIslT :: (MonadIO m, NFData a) => IslT m (Ur a) -> m a
runIslT (IslT f) = do
  ctxPtr <- liftIO c_ctx_alloc
  Ur a <- f (Ctx ctxPtr)
  liftIO $ evaluate (rnf a)    -- force before freeing context!
  liftIO $ c_ctx_free ctxPtr
  return a

runIsl :: NFData a => Isl (Ur a) -> a
runIsl m = unsafePerformIO $ runIslT m
```

The `NFData` constraint on `runIslT` is critical: the result must be fully evaluated before the ISL context is freed, or dangling pointers would result.

#### Linear Do-Notation (`bindings/src/Isl/Linear.hs`)

A custom `QualifiedDo` module that makes `x <- action` bind `x` with multiplicity One:

```haskell
(>>=) :: Monad m => IslT m a %1 -> (a %1 -> IslT m b) %1 -> IslT m b
(>>=) = unsafeCoerce go
  where
    go (IslT m) k = IslT $ \ctx -> do
      a <- m ctx
      unIslT (k a) ctx
```

Used as:
```haskell
{-# LANGUAGE QualifiedDo #-}
import qualified Isl.Linear as Isl

example = runIsl $ Isl.do
  x <- someAction      -- x has multiplicity One
  ...                   -- x must be consumed exactly once
```

#### AutoGen Bindings (e.g., `bindings/src/Isl/Val/AutoGen.hs`)

Auto-generated from C headers. Each function follows this pattern:

```haskell
foreign import ccall "isl_val_2exp" c_twoExp :: Val -> IO Val

twoExp :: (Given Ctx) => Val -> Val
twoExp = \v' -> trace "twoExp" $
    unsafePerformIO $ (return) =<< do
      v <- (return) v'
      let ctx = given :: Ctx
      c_twoExp v
```

The generated code uses `Data.Reflection.Given Ctx` to thread the ISL context, `unsafePerformIO` to present a pure interface, and `Debug.Trace.trace` on every call for debugging.

#### Manual FFI — Callbacks (`bindings/src/Isl/Foreach.hs`)

The codegen cannot handle function-pointer parameters, so iteration functions are hand-written:

```haskell
foreachCollect
  :: (forall b. (a -> Ptr () -> IO CInt) -> IO (FunPtr (a -> Ptr () -> IO CInt)))
  -> (FunPtr (a -> Ptr () -> IO CInt) -> IO CInt)
  -> (a -> IO r)
  -> IO [r]
foreachCollect mkWrapper doForeach process = do
  ref <- newIORef []
  bracket
    (mkWrapper $ \element _user -> do
      result <- process element
      modifyIORef' ref (result :)
      return 0)
    freeHaskellFunPtr
    (\cb -> do _ <- doForeach cb; reverse <$> readIORef ref)
```

This generic pattern is used for `basicSetForeachConstraint`, `setForeachBasicSet`, `mapForeachBasicMap`, `unionSetForeachSet`, `unionMapForeachMap`, and `basicMapForeachConstraint`.

---

### 2. `isl-bindings-hl` — High-Level DSL

**Key idea**: Phantom type parameters encode the parameter space and dimensionality, making it impossible to mix polyhedra from incompatible spaces.

#### Phantom-Indexed Types (`highlevel/src/Isl/HighLevel/BasicSet.hs:44-47`)

```haskell
newtype BasicSet (ps :: [Symbol]) (nDims :: Nat) = BasicSet Isl.BasicSet
newtype BasicSetRef (ps :: [Symbol]) (nDims :: Nat) = BasicSetRef Isl.BasicSetRef
```

For example, `BasicSet '["N", "M"] 3` is a 3-dimensional basic set with parameters N and M.

#### Constraint DSL (`highlevel/src/Isl/HighLevel/Constraints.hs`)

```haskell
data Expr ix = Ix ix | Constant Integer | Mul Integer (Expr ix) | Add (Expr ix) (Expr ix)

data Constraint ix
  = EqualityConstraint (Expr ix)     -- e = 0
  | InequalityConstraint (Expr ix)   -- e >= 0

-- Operators
(>=:) :: Expr ix -> Expr ix -> Constraint ix
(<=:) :: Expr ix -> Expr ix -> Constraint ix
(==:) :: Expr ix -> Expr ix -> Constraint ix
(+:)  :: Expr ix -> Expr ix -> Expr ix
(-:)  :: Expr ix -> Expr ix -> Expr ix
(*:)  :: Integer -> Expr ix -> Expr ix
(&&:) :: ToConjunction c => c ix -> Constraint ix -> Conjunction ix

cst :: Integer -> Expr ix
idx :: ix -> Expr ix
```

Usage:
```haskell
someSet = BS.mkBasicSet @'[] @2 $
  \Nil (x :- y :- Nil) ->
    idx x >=: cst 0 &&: idx x <=: cst 100 &&:
    idx y >=: idx x &&: idx y <=: cst 100
```

#### Parameter Space Merging (`highlevel/src/Isl/HighLevel/Params.hs`)

Binary set operations merge parameter spaces via a sorted-merge type family:

```haskell
type family Union (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  Union '[] ys = ys
  Union xs '[] = xs
  Union (x ': xs) (y ': ys) = UnionCase (CmpSymbol x y) x xs y ys
```

So `union :: Set ps1 n -> Set ps2 n -> IslT m (Set (Union ps1 ps2) n)` correctly tracks that unioning a set with params `'["M"]` and one with `'["N"]` produces `'["M", "N"]`.

#### Pure Representations (`highlevel/src/Isl/HighLevel/Pure.hs`)

ISL objects can be decomposed into pure Haskell data that doesn't hold ISL pointers:

```haskell
newtype PConjunction (ps :: [Symbol]) (n :: Nat) = PConjunction (Conjunction SetIx)
newtype PDisjunction (ps :: [Symbol]) (n :: Nat) = PDisjunction [PConjunction ps n]
newtype PMapConjunction (ps :: [Symbol]) (nIn :: Nat) (nOut :: Nat) = PMapConjunction (Conjunction MapIx)
```

These can be built without ISL at all:
```haskell
domain :: PConjunction '["N"] 2
domain = mkPConjunction @'["N"] @2 $
  \(n :- Nil) (x :- y :- Nil) ->
    idx x >=: cst 0 &&: idx x <=: idx n -: cst 1
    &&: idx y >=: cst 0 &&: idx y <=: idx x
```

#### Length-Indexed Index Lists (`highlevel/src/Isl/HighLevel/Indices.hs`)

```haskell
data IxList :: Nat -> Type -> Type where
  Nil :: IxList 0 a
  (:-) :: a -> IxList n a -> IxList (n+1) a
```

Used in the DSL lambdas: `\(x :- y :- Nil) -> ...`. The GADT ensures the number of variables matches the type-level dimension count.

---

### 3. `isl-scan` — Polyhedral Scanning

**Key idea**: Compile a polyhedron into a loop nest descriptor (pure Haskell, no ISL at runtime), then enumerate integer points.

#### Core Data Types (`scan/src/Isl/Scan/Types.hs`)

```haskell
data AffineBound = AffineBound
  { abLoopCoeffs  :: [(Integer, Int)]   -- (coefficient, outer loop var index)
  , abParamCoeffs :: [(Integer, Int)]   -- (coefficient, parameter index)
  , abConstant    :: Integer
  , abDivisor     :: Integer            -- always positive
  }

data LoopLevel = LoopLevel
  { llDim         :: Int
  , llLowerBounds :: [AffineBound]      -- x_k >= max(ceil(lower_i))
  , llUpperBounds :: [AffineBound]      -- x_k <= min(floor(upper_j))
  , llEquality    :: Maybe AffineBound  -- x_k = expr (when present, bounds ignored)
  , llStride      :: Integer            -- usually 1
  }

data LoopNest (ps :: [Symbol]) (n :: Nat) = LoopNest
  { lnLevels :: [LoopLevel], lnParams :: Int, lnDims :: Int }

newtype Scanner (ps :: [Symbol]) (n :: Nat) = Scanner [LoopNest ps n]
```

#### Boulet-Feautrier Algorithm (`scan/src/Isl/Scan/Build.hs`)

For each dimension k (outermost to innermost):
1. Partition constraints into those involving x_k and those that don't
2. Classify by coefficient sign: positive = lower bound, negative = upper bound
3. Normalize each into an `AffineBound` by dividing out the coefficient of x_k

```haskell
mkScanner :: PDisjunction ps n -> Scanner ps n
mkScanner (PDisjunction pcs) = Scanner (map mkLoopNest pcs)
```

#### Two Enumeration Strategies

**Nested loops** (`scan/src/Isl/Scan/Enumerate.hs`):
```haskell
scanPoints :: Scanner ps n -> Vec (Length ps) Integer -> [Vec n Integer]
scanFold   :: Scanner ps n -> Vec (Length ps) Integer -> (a -> Vec n Integer -> a) -> a -> a
scanForM_  :: Monad m => Scanner ps n -> Vec (Length ps) Integer -> (Vec n Integer -> m ()) -> m ()
```

**FSM (finite state machine)** (`scan/src/Isl/Scan/FSM.hs`):
Maintains an explicit iteration vector and advances lexicographically:
1. Start at lexmin
2. At each step, find the innermost non-saturated dimension (carry dimension)
3. Increment it, reset all inner dims to their lower bounds
4. Stop when all dimensions are saturated (at lexmax)

```haskell
scanFSM     :: LoopNest ps n -> Vec (Length ps) Integer -> [Vec n Integer]
scanFoldFSM :: LoopNest ps n -> Vec (Length ps) Integer -> (a -> Vec n Integer -> a) -> a -> a
```

The FSM approach naturally produces points in lexicographic order and exposes the "face" structure (which constraints are saturated at each step).

---

### 4. `isl-typelevel` — Type-Level Polyhedral Constraints

**Key idea**: Encode polyhedral constraints entirely at the GHC type level, so that properties like subset inclusion can be checked at compile time.

#### Type-Level Signed Integers (`typelevel/src/Isl/TypeLevel/Expr.hs`)

GHC has no type-level `Integer`, so:

```haskell
data Z = Pos Nat | Neg Nat    -- 'Pos 3 = +3, 'Neg 2 = -2
```

#### Type-Level Expressions

```haskell
data TExpr
  = TDim Nat           -- dimension variable by index
  | TParam Symbol      -- parameter variable by name
  | TConst Z           -- integer constant
  | TAdd TExpr TExpr   -- addition
  | TMul Z TExpr       -- scalar multiplication
```

With sugar operators:
```haskell
type family (a :: TExpr) +. (b :: TExpr) :: TExpr where  a +. b = 'TAdd a b
type family (a :: TExpr) -. (b :: TExpr) :: TExpr where  a -. b = 'TAdd a ('TMul ('Neg 1) b)
type family (k :: Z)     *. (a :: TExpr) :: TExpr where  k *. a = 'TMul k a
```

#### Type-Level Constraints and Proof Obligations (`typelevel/src/Isl/TypeLevel/Constraint.hs`)

```haskell
data TConstraint = TEq TExpr | TGe TExpr

type family (a :: TExpr) >=. (b :: TExpr) :: TConstraint where  a >=. b = 'TGe (a -. b)
type family (a :: TExpr) <=. (b :: TExpr) :: TConstraint where  a <=. b = 'TGe (b -. a)
type family (a :: TExpr) ==. (b :: TExpr) :: TConstraint where  a ==. b = 'TEq (a -. b)
```

Proof obligation classes (solved by the plugin):
```haskell
class IslSubset   ps n cs1 cs2          -- cs1 ⊆ cs2
class IslNonEmpty ps n cs               -- cs ≠ ∅
class IslEqual    ps n cs1 cs2          -- cs1 = cs2
class IslDomainOf ps ni no mapCs domCs  -- domCs = domain(mapCs)
```

#### Compile-Time Validation

Every constraint is validated against the parameter list and dimension count:
```haskell
type family ValidExpr (ps :: [Symbol]) (n :: Nat) (e :: TExpr) :: Constraint where
  ValidExpr _  n ('TDim d)   = CheckDim d n    -- d < n
  ValidExpr ps _ ('TParam s) = CheckParam s ps  -- s ∈ ps
  ValidExpr _  _ ('TConst _) = ()
  ValidExpr ps n ('TAdd a b) = (ValidExpr ps n a, ValidExpr ps n b)
  ValidExpr ps n ('TMul _ a) = ValidExpr ps n a
```

Using `'TDim 5` in a 2-dimensional space produces a clear `TypeError`:
> "Dimension index 5 out of bounds for 2-dimensional space"

#### Reification (`typelevel/src/Isl/TypeLevel/Sing.hs`)

Type-level constraints can be reified to value-level pure representations:

```haskell
reifyBasicSet :: (AllValid ps n cs, ReifyTConstraints cs SetIx) => PConjunction ps n
reifyBasicSet = PConjunction (Conjunction (reifyTConstraints @cs @SetIx))
```

---

### 5. `isl-plugin` — GHC Typechecker Plugin

**Key idea**: Intercept `IslSubset`, `IslNonEmpty`, and `IslEqual` constraints during type-checking, build actual ISL objects from the type-level data, call the ISL C library, and provide evidence if the check passes.

#### Plugin Structure (`plugin/src/Isl/Plugin.hs`)

```haskell
plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just islTcPlugin
  , pluginRecompile = purePlugin
  }
```

The plugin:
1. **Initializes**: Allocates an ISL context, looks up all relevant GHC `TyCon`s for `TDim`, `TParam`, `TConst`, `TAdd`, `TMul`, `TEq`, `TGe`, `Pos`, `Neg`
2. **Solves**: For each wanted constraint, walks the GHC type trees to extract value-level `Constraint SetIx` lists, builds ISL `Set`s programmatically, and calls `isl_set_is_subset` / `isl_set_is_empty` / `isl_set_is_equal`
3. **Provides evidence**: On success, constructs `EvTerm` evidence via `evDataConApp`
4. **Stops**: Frees the ISL context

#### Type Tree Walking

The plugin manually unfolds promoted lists and matches promoted data constructors:

```haskell
unfoldTypeList :: Type -> [Type]
unfoldTypeList ty = case splitTyConApp_maybe ty of
  Just (tc, [_k, x, xs]) | getOccString tc == ":" -> x : unfoldTypeList xs
  Just (tc, _)           | getOccString tc == "'[]" -> []
  _ -> []
```

And reifies type-level expressions by matching against the cached `TyCon`s:
```haskell
reifyTExpr env paramNames ty = case splitTyConApp_maybe ty of
  Just (tc, [nTy]) | tc == envTDim env -> Ix . SetDim <$> extractNat nTy
  Just (tc, [sTy]) | tc == envTParam env -> do
    name <- extractSymbol sTy
    idx  <- elemIndex name paramNames
    Just $ Ix (SetParam idx)
  -- ...
```

---

### 6. `codegen` — Binding Generator

**Key idea**: Parse ISL C headers, classify function parameters by ISL's `__isl_take` / `__isl_keep` / `__isl_give` annotations, and emit Haskell FFI code.

The codegen is a standalone Haskell executable (`codegen/IslCodegen.hs`, ~500 lines) that:
1. Reads ISL C header files from the ISL installation directory
2. Parses function signatures
3. Generates `AutoGen.hs` modules with FFI imports and pure wrappers

The generated code (12 modules, ~10,900 lines total) uses `Given Ctx` + `unsafePerformIO` for a pure interface, with `Debug.Trace.trace` calls on every function.

---

### 7. `isl-test` — Examples and Test Suite

#### Executables

| Name | Description | Key technique |
|------|-------------|---------------|
| `isl-test` | Basic ISL operations | Linear `QualifiedDo`, modulo via existential variables |
| `matmul` | Matrix multiplication | Polyhedral scanning, parametric domains |
| `tiled-matmul` | Tiled matrix multiplication | 5D tiling domain, FSM iteration, carry detection |
| `prefix-sum` | Prefix sum (scanl) | 2D domain, mutable vectors |
| `typelevel-demo` | Compile-time proofs | GHC plugin, type-level subset proof |

#### Test Suite

The test suite (`isl-test/test/`) uses Tasty with HUnit and QuickCheck:

- **BasicSet tests**: `mkBasicSet`, `intersect`, `fromString` round-trip, `decomposeBS`, property tests for commutativity and idempotency
- **Set tests**: `union`, `intersect`, `subtract`, `isEmpty`, De Morgan's law, subset relationships
- **BasicMap tests**: `fromString`, `domain`, `range`, `isEmpty`, decompose round-trip
- **Map tests**: `union`, `intersect`, `subtract`, `domain`
- **Scan tests**: Rectangle, triangle, parametric, single point, empty set, ISL round-trip, cross-check (nested loops vs FSM produce identical points)

---

## Design Decisions

### 1. Linear Types for Memory Safety

ISL's C API uses reference-counting with `__isl_take`/`__isl_keep`/`__isl_give` ownership annotations. The bindings use GHC's `LinearTypes` extension to enforce these at the Haskell level:

- **Owned values** (`BasicSet`, `Set`, etc.) have multiplicity `One` — they must be consumed exactly once
- **Borrowed references** (`BasicSetRef`, `SetRef`, etc.) are unrestricted — safe for queries
- `Ur a` ("unrestricted") wraps values that escape linear tracking

This means use-after-free and double-free are compile-time errors, not runtime crashes.

### 2. The `unsafeCoerce` Pattern

Every linear operation is implemented via `unsafeCoerce` to bypass the linearity checker, then delegates to the "real" implementation:

```haskell
intersect :: BasicSet ps n %1 -> BasicSet ps n %1 -> IslT m (BasicSet ps n)
intersect = unsafeCoerce go
  where
    go :: BasicSet ps n -> BasicSet ps n -> IslT m (BasicSet ps n)
    go (BasicSet bs1) (BasicSet bs2) = BasicSet <$> withCtx (BS.intersect bs1 bs2)
```

**Why**: GHC's linearity checker cannot verify that pattern-matching on a `newtype` and passing the inner value to an ISL function constitutes a valid linear use. The `unsafeCoerce` erases the multiplicity annotation, and the programmer guarantees correctness. This is the standard idiom in the linear Haskell ecosystem (see `linear-base`).

### 3. `Given Ctx` via `Data.Reflection`

Auto-generated code uses `Data.Reflection.Given` to implicitly thread the ISL context:

```haskell
twoExp :: (Given Ctx) => Val -> Val
twoExp v = unsafePerformIO $ c_twoExp v
```

The high-level code bridges this via `withCtx`:

```haskell
withCtx :: Monad m => (Given Ctx => a) -> IslT m a
withCtx f = IslT $ \ctx -> let !result = give ctx f in return result
```

**The bang pattern is critical**: `give` provides the `Given` dictionary only within its scope. Without `!`, lazy evaluation could defer the `unsafePerformIO` call past the `give` scope, causing the `given :: Ctx` to resolve to garbage.

### 4. Sorted Parameter Lists

ISL canonically orders parameters alphabetically. The bindings mirror this: `'["K", "M", "N"]` is the parameter list for a set with parameters K, M, N. The `Union` type family performs a sorted merge, matching ISL's behavior when combining sets with different parameter spaces.

**Consequence**: When building a matmul domain with parameters K, M, N, the parameter indices are K=0, M=1, N=2 (alphabetical), not the order you might naturally write them:

```haskell
-- Parameters (alphabetical): K=0, M=1, N=2
matmulDomain = mkPConjunction @'["K","M","N"] @3 $
  \(kp :- mp :- np :- Nil) (i :- j :- k :- Nil) ->
    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1   -- np is N, index 2
    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1
```

### 5. Two-Level Type Safety

The bindings provide safety at two levels:

1. **Value-level** (runtime): Phantom types on `BasicSet ps n`, `Set ps n`, `Map ps ni no` prevent mixing incompatible spaces. Operations like `intersect` require matching dimensions. `Union` at the type level correctly merges parameter spaces.

2. **Type-level** (compile-time): The `isl-typelevel` + `isl-plugin` packages go further — you can state polyhedral properties as GHC type constraints and have them verified at compile time by the actual ISL library.

### 6. Pure Scanning (No ISL at Runtime)

The `isl-scan` package decomposes ISL sets into pure Haskell `PConjunction`/`PDisjunction` data, then compiles them into `Scanner` loop nest descriptors. Point enumeration is entirely in Haskell — no ISL calls at runtime. This means:

- The scanner can be serialized, sent across the network, etc.
- No foreign library needed at scan time
- The FSM approach gives lexicographic order and face structure for free

### 7. Monad Transformer

`IslT m a` rather than just `Isl a` allows composing ISL operations with other effects:

```haskell
newtype IslT m a = IslT { unIslT :: Ctx -> m a }
type Isl = IslT IO
```

You can stack `IslT` on `StateT`, `ExceptT`, etc.

---

## Concrete Examples

### Example 1: Modulo via Existential Variables (`isl-test/src/Main.hs`)

ISL doesn't have a modulo operator, but you can express "x is even" by introducing an existential variable k and asserting x = 2k, then projecting k out:

```haskell
evenXs :: MonadIO m => IslT m (BasicSet '[] 2)
evenXs = Isl.do
  bs3 <- BS.mkBasicSet @'[] @3 $
    \Nil (x :- _ :- k :- Nil) -> toConjunction $
      idx x ==: 2 *: idx k
  BS.eliminateLast bs3
```

The full program demonstrates the linear ownership protocol:

```haskell
main = do
  let (s1, s2, s3) = runIsl $ Isl.do
        ex <- evenXs
        ss <- someSet
        (Ur s1, ex') <- BS.borrowBS ex BS.bsetToString
        (Ur s2, ss') <- BS.borrowBS ss BS.bsetToString
        isect <- BS.intersect ex' ss'
        (Ur s3, isect') <- BS.borrowBS isect BS.bsetToString
        BS.freeBS isect'
        Isl.pure (Ur (s1, s2, s3))
  putStrLn s1; putStrLn s2; putStrLn s3
```

Every ISL object is consumed exactly once: `ex'` and `ss'` go into `intersect`, `isect'` is freed, and the strings escape via `Ur`.

### Example 2: Matrix Multiplication (`isl-test/src/Matmul.hs`)

```haskell
matmulDomain :: PConjunction '["K", "M", "N"] 3
matmulDomain = mkPConjunction @'["K","M","N"] @3 $
  \(kp :- mp :- np :- Nil) (i :- j :- k :- Nil) ->
    idx i >=: cst 0 &&: idx i <=: idx np -: cst 1
    &&: idx j >=: cst 0 &&: idx j <=: idx mp -: cst 1
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1
```

The scanner drives the computation:

```haskell
let scanner = mkScanner (PDisjunction [matmulDomain])
    params = mkVec @3 [fromIntegral k, fromIntegral m, fromIntegral n]

scanForM_ scanner params $ \(Vec [ii, jj, kk]) -> do
  old <- readArray c (i, j)
  writeArray c (i, j) (old + matA ! (i, kv) * matB ! (kv, j))
```

The pretty-printer shows the generated loop nest structure:
```
for i = 0 to N-1:
  for j = 0 to M-1:
    for k = 0 to K-1:
      body(i, j, k)
```

### Example 3: Tiled Matrix Multiplication (`isl-test/src/TiledMatmul.hs`)

A 5-dimensional polyhedral domain encodes tiling:

```haskell
tiledMatmulDomain :: Integer -> PConjunction '["K", "M", "N"] 5
tiledMatmulDomain t = mkPConjunction @'["K","M","N"] @5 $
  \(kp :- mp :- np :- Nil) (ti :- tj :- i :- j :- k :- Nil) ->
    -- Tile index bounds
        idx ti >=: cst 0 &&: t *: idx ti <=: idx np -: cst 1
    &&: idx tj >=: cst 0 &&: t *: idx tj <=: idx mp -: cst 1
    -- i within tile and within N
    &&: idx i >=: t *: idx ti &&: idx i <=: t *: idx ti +: cst (t - 1)
    &&: idx i <=: idx np -: cst 1
    -- j within tile and within M
    &&: idx j >=: t *: idx tj &&: idx j <=: t *: idx tj +: cst (t - 1)
    &&: idx j <=: idx mp -: cst 1
    -- k range
    &&: idx k >=: cst 0 &&: idx k <=: idx kp -: cst 1
```

The `min` in the tiled loop bounds (`i <= min(T*ti+T-1, N-1)`) is handled naturally — two upper bounds in the scanner, and it takes the minimum.

This example also demonstrates the FSM scanner's carry detection:
```haskell
detectCarry :: [Integer] -> [Integer] -> Int
detectCarry prev next = go 0
  where
    go i | i >= length prev = length prev - 1
         | prev !! i /= next !! i = i
         | otherwise = go (i + 1)
```

### Example 4: Prefix Sum (`isl-test/src/PrefixSum.hs`)

`out[i] = sum(inp[0..i-1])` as a 2D polyhedral scan:

```haskell
-- Domain: { [i, j] : 1 <= i <= N, 0 <= j < i }
domain = mkPConjunction @'["N"] @2 $
  \(n :- Nil) (i :- j :- Nil) ->
    idx i >=: cst 1 &&: idx i <=: idx n
    &&: idx j >=: cst 0 &&: idx j <=: idx i -: cst 1
```

### Example 5: Compile-Time Proofs (`isl-test/src/TypeLevelDemo.hs`)

```haskell
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}

type Triangle =
  '[ 'TDim 0  >=. 'TConst ('Pos 0)
   , 'TParam "N" >=. 'TDim 0
   , 'TDim 1  >=. 'TConst ('Pos 0)
   , 'TDim 0  >=. 'TDim 1
   ]

type Rectangle =
  '[ 'TDim 0  >=. 'TConst ('Pos 0)
   , 'TParam "N" >=. 'TDim 0
   , 'TDim 1  >=. 'TConst ('Pos 0)
   , 'TParam "N" >=. 'TDim 1
   ]

-- ISL proves at compile time: Triangle ⊆ Rectangle.
-- If this relationship didn't hold, the module would fail to compile!
proofSubset :: IslSubset '["N"] 2 Triangle Rectangle => ()
proofSubset = ()
```

The proof is reified to runtime for display:
```haskell
instance ParamIndex "N" where paramIndex = 0

triangleValue :: PConjunction '["N"] 2
triangleValue = reifyBasicSet @'["N"] @2 @Triangle
```

---

## Weird Things

### 1. `Debug.Trace.trace` in every auto-generated function

**File**: `bindings/src/Isl/Val/AutoGen.hs:25`, and every other AutoGen module.

```haskell
getCtx :: (Given Ctx) => Val -> Ctx
getCtx = \val' -> trace "getCtx" $
    unsafePerformIO $ (return) =<< do ...
```

Every auto-generated wrapper includes `trace "functionName"`, which prints to stderr on every ISL call. This is a debugging aid left permanently in the generated code. There is no way to disable it short of editing the codegen or post-processing the output.

### 2. Redundant `(return) =<<` in generated code

**File**: `bindings/src/Isl/Val/AutoGen.hs:26`

```haskell
unsafePerformIO $ (return) =<< do
  v <- (return) v'
  ...
```

`(return) =<< action` is `action >>= return`, which is just `action` by the monad identity law. Similarly, `v <- (return) v'` is a no-op binding. These appear in every generated function — the codegen emits them uniformly as part of its template without optimizing them away.

### 3. `{-# LANGUAGE Strict #-}` on Types.hsc and Instances.hs

**Files**: `bindings/src/Isl/Types.hsc:6`, `bindings/src/Isl/Instances.hs:6`

The `Strict` language extension makes all bindings strict by default. Combined with `unsafePerformIO` in the instances, this could cause unexpected evaluation ordering. The interaction between `Strict`, `unsafePerformIO`, and `unsafeCoerce` in the same module is a minefield for subtle bugs.

### 4. Massive `unsafeCoerce` usage

**File**: `bindings/src/Isl/Instances.hs` (entire file), `bindings/src/Isl/Linear.hs:38,51,72`, `highlevel/src/Isl/HighLevel/BasicSet.hs:54,57,61,122,130,149,165,202,212,222`, and virtually every function in every HighLevel module.

Every linear operation uses `unsafeCoerce` to erase multiplicity annotations. While this is the accepted idiom in linear Haskell, the sheer density is remarkable — there are over **60 uses of `unsafeCoerce`** across the hand-written code alone. A single mistake (e.g., forgetting to consume a value in the `go` helper) would silently break the linear safety guarantee.

### 5. `isEmpty` after `consume` pattern

**File**: `highlevel/src/Isl/HighLevel/BasicSet.hs:221-228`

```haskell
consumingIsEmpty :: BasicSet ps n %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go (BasicSet bs) = do
      r <- withCtx (BS.isEmpty bs)
      freeM bs           -- frees bs AFTER using it in isEmpty
      return (Ur r)
```

This calls `BS.isEmpty bs` (which is `__isl_keep` — borrows) and then `freeM bs`. The `isEmpty` result is already computed, so the free is safe. But the ISL `isEmpty` function internally accesses the pointer, and the free happens *after* — the sequencing here relies on monadic ordering in IO, which is correct but fragile.

### 6. `consumingIsEqual` calls `freeM` twice with semicolons

**File**: `highlevel/src/Isl/HighLevel/Set.hs:204-207`

```haskell
go (Set s1) (Set s2) = do
  r <- withCtx (S.isEqual s1 s2)
  freeM s1; freeM s2
  return (Ur r)
```

After `S.isEqual` (which is `__isl_keep` on both arguments), both sets are freed. The use of `;` on the same line is cosmetic but unusual in Haskell. More importantly, the isEqual result may hold references into the set's internal data — though in practice ISL's `is_equal` returns a C `isl_bool`, which is a plain integer, so this is safe.

### 7. `Foreach.hs` casts borrowed refs to owned types

**File**: `bindings/src/Isl/Foreach.hs:111,117,124,131`

```haskell
basicSetGetSpace :: BasicSetRef -> IO Space
basicSetGetSpace (BasicSetRef bsPtr) = c_basic_set_get_space (BasicSet bsPtr)
```

A `BasicSetRef` (borrowed) is unwrapped and rewrapped as a `BasicSet` (owned) just to call the FFI function. This works because the C function is `__isl_keep` on the basic set, but the Haskell types don't enforce this — `c_basic_set_get_space` is declared as taking an owned `BasicSet`, when semantically it borrows.

### 8. `.gitmodules` exists but is empty

**File**: `.gitmodules`

The file exists (and is tracked by git status as untracked) but contains nothing. This is a leftover from a removed submodule.

### 9. `azure-pipelines.yml` uses Ubuntu 16.04

**File**: `azure-pipelines.yml`

```yaml
pool:
  vmImage: 'Ubuntu 16.04'
```

Ubuntu 16.04 reached end-of-life in April 2021. This CI configuration is likely non-functional. The project also has a `flake.nix` that's clearly more up-to-date.

### 10. `isl-codegen` is not in `cabal.project` but IS in the Nix flake

**File**: `cabal.project` includes `codegen/` but the codegen is built separately in `flake.nix:27-28` via `callCabal2nix`. The codegen is run during the Nix build to produce the `AutoGen.hs` files — it's part of the build pipeline, not a library dependency.

### 11. The `Ur` type duplicates `linear-base`'s `Ur`

**File**: `bindings/src/Isl/Monad.hs:31-32`

```haskell
data Ur a where
  Ur :: a -> Ur a
```

This is identical to `Prelude.Linear.Ur` from the `linear-base` package. The project defines its own to avoid the dependency.

### 12. `Vec` is a plain list with phantom length

**File**: `scan/src/Isl/Scan/Types.hs:29`

```haskell
newtype Vec (n :: Nat) a = Vec { toList :: [a] }
```

The length is not enforced structurally (unlike a GADT-based vector). `mkVec` validates at runtime, but `unsafeVec` does not. Internal code uses `unsafeVec` extensively, meaning length mismatches would produce silent bugs rather than compile errors.

### 13. `IxList` uses `unsafeCoerce` to bypass length tracking

**File**: `highlevel/src/Isl/HighLevel/Indices.hs:20-21`

```haskell
coerceIxList :: forall n ix. SomeIxList ix -> IxList n ix
coerceIxList (SomeIxList list) = unsafeCoerce list
```

The existentially-quantified `SomeIxList` is coerced to any `IxList n` the caller wants. This is correct when the caller knows the length, but there's no static guarantee.

### 14. `nix/versions.json` references `inline-rust`

**File**: `nix/versions.json:9-15`

```json
"inline-rust": {
  "owner": "nickg",
  "repo": "ghc-inline-rust",
  ...
}
```

A pinned Rust-inline dependency appears in the legacy Nix config but isn't used anywhere in the current codebase. This is a vestige of a previous experiment.

### 15. `Isl.HighLevel` module is empty

**File**: `highlevel/src/Isl/HighLevel.hs`

The top-level re-export module exists but exports nothing. It's a placeholder that was never filled in.

### 16. `MapDim` legacy alias

**File**: `highlevel/src/Isl/HighLevel/Constraints.hs:96`

```haskell
type MapDim = MapIx
```

A legacy type alias is kept for backward compatibility, with a comment "use `MapIx` for new code." Given the project's experimental status and that this is the only alias, it could be removed.

### 17. `IslDomainOf` class is declared but never solved

**File**: `typelevel/src/Isl/TypeLevel/Constraint.hs:136-137`

```haskell
class IslDomainOf ps ni no mapCs domainCs
```

This proof obligation class is declared but the plugin (`plugin/src/Isl/Plugin.hs`) only handles `IslSubset`, `IslNonEmpty`, and `IslEqual`. `IslDomainOf` would require map construction, which the plugin doesn't implement yet.

### 18. Plugin uses string comparison for promoted list constructors

**File**: `plugin/src/Isl/Plugin.hs:376-379`

```haskell
unfoldTypeList ty = case splitTyConApp_maybe ty of
  Just (tc, [_k, x, xs]) | getOccString tc == ":" -> x : unfoldTypeList xs
  Just (tc, _)           | getOccString tc == "'[]" -> []
```

Instead of looking up the promoted `(:)` and `'[]` constructors properly (via `lookupOrig`), the plugin matches them by their `OccName` strings. This is fragile — if GHC ever changes the string representation, it breaks silently.

### 19. The `bindings/test/Simple.hs` test is not in any cabal file

**File**: `bindings/test/Simple.hs` exists but `bindings/isl-bindings.cabal` has no `test-suite` stanza. This test file is orphaned and never compiled or run.

### 20. No `Eq`/`Ord`/`Show` instances for owned ISL types

The owned types (`BasicSet`, `Set`, `Map`, etc. in `Isl.Types`) derive only `Storable`. There's no `Show` instance — you must `borrow` the value and call `bsetToString` etc. explicitly. This makes debugging in GHCi painful.

### 21. `consume` uses `unsafePerformIO` and can be deferred by laziness

**File**: `bindings/src/Isl/Instances.hs:24-25`

```haskell
instance Consumable Aff where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_aff_free x)
```

The `consume` function frees memory via `unsafePerformIO`. If the call site is lazy, the free could be deferred indefinitely. This is why `freeM` exists as a monadic alternative:

```haskell
-- bindings/src/Isl/Monad.hs:103-104
freeM :: (MonadIO m, Consumable a) => a -> IslT m ()
freeM x = IslT $ \_ -> liftIO $ evaluate (consume x)
```

---

## Build System

### Nix Flake (`flake.nix`)

The primary build system. Key features:

1. **ISL 0.27** is built from source (nixpkgs ships 0.20 for GCC compatibility):
   ```nix
   isl_0_27 = pkgs.isl.overrideAttrs (old: rec {
     version = "0.27";
     src = pkgs.fetchurl {
       urls = [ "mirror://sourceforge/libisl/isl-${version}.tar.xz" ];
       hash = "sha256-bYurtZ57Zy6Mt4cOh08/e4E7bgDmrz+LBPdXmWVkPVw=";
     };
   });
   ```

2. **Code generation pipeline**: The codegen executable is built first, then run against ISL 0.27 headers to produce `AutoGen.hs` files, which are merged with the manual source:
   ```nix
   isl-bindings-srcgen = pkgs.stdenv.mkDerivation {
     buildInputs = [ isl-codegen isl_0_27 ];
     buildPhase = ''isl-codegen ${isl_0_27} .'';
   };
   isl-bindings-src = pkgs.stdenv.mkDerivation {
     buildPhase = ''
       cp -r ${./bindings}/* .
       cp -r ${isl-bindings-srcgen}/* .
     '';
   };
   ```

3. **Dev shell** with cabal, ghcid, Hoogle, and ISL 0.27:
   ```nix
   devShells.default = haskellPackages.shellFor {
     packages = ps: [ ps.isl-bindings ps.isl-bindings-hl ps.isl-scan
                      ps.isl-typelevel ps.isl-plugin ps.isl-test ];
     buildInputs = [ pkgs.cabal-install pkgs.haskellPackages.ghcid isl_0_27 ];
     withHoogle = true;
   };
   ```

### Legacy Nix (`default.nix`, `shell.nix`, `nix/`)

The pre-flake Nix setup still exists. Uses pinned nixpkgs from `nix/versions.json` and overlays from `nix/overlays.nix`. The `azure-pipelines.yml` CI still references the legacy `nix-build` path.

### Cabal Workspace (`cabal.project`)

```
packages:
  codegen/
  bindings/
  highlevel/
  scan/
  typelevel/
  plugin/
  isl-test/
```

All 7 packages are in a single workspace. Local development with `cabal build all` works if ISL 0.27 is installed and the AutoGen files have been generated.
