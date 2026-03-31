# ISL Concepts: From First Principles to Advanced Composition

A comprehensive reference for the Integer Set Library (ISL), ordered from the most fundamental building blocks to the most composite structures.

---

## Level 0: Context

### `isl_ctx` -- The Runtime Environment

Every ISL object lives inside a **context**. The context manages memory, error handling, and global options. All objects that interact must share the same context.

```c
isl_ctx *ctx = isl_ctx_alloc();
// ... all ISL work happens here ...
isl_ctx_free(ctx);
```

In the Haskell bindings, `IslT` (or `Isl`) monad manages the context automatically:

```haskell
runIslT $ do
  s <- mkBasicSet @'["N"] @2 $ \[i, j] [n] ->
    [idx i >=: cst 0, idx i <=: idx n, idx j >=: cst 0]
  -- s is freed when IslT scope ends
```

### Memory Management Conventions

ISL uses reference counting with three annotation conventions on C functions:

| Annotation     | Meaning                                           |
|----------------|---------------------------------------------------|
| `__isl_give`   | Caller receives ownership, must eventually free   |
| `__isl_take`   | Callee consumes the argument (caller loses it)    |
| `__isl_keep`   | Callee borrows read-only (caller retains ownership) |

In Haskell, these map to **linear types**: owned values are consumed exactly once, borrowed references (`Ref`) are unrestricted.

---

## Level 1: Scalars and Identifiers

### `isl_val` -- Arbitrary-Precision Numbers

ISL never uses C `int` internally. All numeric values are `isl_val`: arbitrary-precision integers or rationals. Special values include NaN, +infinity, -infinity.

```
val = 42
val = 3/7
val = infty
val = NaN
```

Operations: add, sub, mul, div, mod, gcd, min, max, abs, ceil, floor, trunc, comparisons.

### `isl_id` -- Named Identifiers

An identifier is a name (string) optionally paired with a user pointer. Used to name parameters, dimension tuples (statement names), and schedule tree marks.

```
id = "S"      -- a statement name
id = "N"      -- a parameter name
```

Two ids are equal iff they have the same name **and** the same user pointer.

---

## Level 2: Spaces -- The Dimensional Framework

### `isl_space` -- Structure Without Content

A **space** describes the shape of a mathematical object without specifying any constraints or values. It declares:

- **Parameters**: symbolic constants (like `N`, `M`) shared across all objects in a computation
- **Dimensions**: the variables, categorized by role
- **Tuple identifiers**: optional names for the domain and range tuples

There are three kinds of spaces:

#### Set Space

For sets of integer points. Has parameters and **set dimensions**.

```
[N, M] -> { [i, j, k] }
 ^^^^^      ^^^^^^^^^
 params     3 set dims
```

#### Map Space

For relations between integer tuples. Has parameters, **input dimensions** (domain), and **output dimensions** (range).

```
[N] -> { [i, j] -> [i', j'] }
        ^^^^^^^    ^^^^^^^^^
        2 in dims  2 out dims
```

#### Parameter Space

Zero-dimensional, carries only parameters. Used for constraints on parameters alone.

```
[N, M] -> { [] }
```

### Dimension Types (`isl_dim_type`)

Every dimension in ISL has a **type** that determines its role:

| Type           | Meaning                                             |
|----------------|-----------------------------------------------------|
| `isl_dim_cst`  | The constant term (dimension 0 of the constant)     |
| `isl_dim_param` | A symbolic parameter (e.g., `N`, `M`)              |
| `isl_dim_set`  | A set dimension (for sets)                          |
| `isl_dim_in`   | An input/domain dimension (for maps)                |
| `isl_dim_out`  | An output/range dimension (for maps)                |
| `isl_dim_div`  | A local/existentially quantified dimension (see below) |
| `isl_dim_all`  | All dimensions (used in queries)                    |

### Space Operations

| Operation   | Description                                                      |
|-------------|------------------------------------------------------------------|
| `domain`    | Extract the domain space of a map space                          |
| `range`     | Extract the range space of a map space                           |
| `params`    | Extract parameter-only space                                     |
| `reverse`   | Swap input and output                                            |
| `join`      | Combine two map spaces (range of first = domain of second)       |
| `wrap`      | Turn a map space `A -> B` into a set space `[A, B]`             |
| `unwrap`    | Inverse of wrap                                                  |
| `curry`     | `(A, B) -> C` becomes `A -> (B -> C)`                          |
| `uncurry`   | Inverse of curry                                                 |

### `isl_local_space` -- Space with Existential Variables

A **local space** extends a space with **div dimensions** (existentially quantified integer division variables). These are needed for creating affine expressions that involve integer division. See [Div Dimensions](#div-dimensions-existentially-quantified-variables) below.

---

## Level 3: Div Dimensions -- Existentially Quantified Variables

### What Are Div Dimensions?

Div dimensions (also called **local variables**, **existential variables**, or **integer divisions**) are one of ISL's most subtle and powerful features. They arise because ISL works over the **integers**, not the rationals, and integer arithmetic requires expressing things like "x is even" or "floor(x/3)".

A div dimension represents a value defined by an **integer division expression**:

```
e = floor((a0 + a1*x1 + a2*x2 + ... + an*xn) / d)
```

where `d > 0` and `a0, a1, ..., an` are integer coefficients. The variable `e` is **existentially quantified**: it exists as a witness but is not a "real" dimension of the set.

### Why Are They Needed?

Consider the set of all even integers between 0 and N:

```
{ [i] : 0 <= i <= N and exists e : i = 2*e }
```

ISL represents this using a div dimension `e = floor(i/2)`:

```
[N] -> { [i] : 0 <= i <= N and i = 2*floor(i/2) }
```

The div dimension `floor(i/2)` is local -- it doesn't appear in the space signature `[i]`, but it participates in the constraints.

### How ISL Stores Div Dimensions

Each div dimension is stored as a **division expression** in the local space:

```
div[k] = floor((c_0 + c_param * params + c_in * in + c_out * out + c_div * divs) / denominator)
```

The key properties:
- Divs can depend on other divs (nested floor divisions)
- Divs are implicitly existentially quantified: `exists e_k : e_k = floor(...)`
- A div with all-zero coefficients is "unknown" / not yet determined

### Concrete Examples

**Modular arithmetic** -- "i is divisible by 3":

```
{ [i] : exists e : i = 3*e }
-- Internally: div[0] = floor(i/3), constraint: i = 3*div[0]
```

**Floor division** -- "j = floor(i/4)":

```
{ [i] -> [j] : j = floor(i/4) }
-- Internally: div[0] = floor(i/4), constraint: j = div[0]
```

**Modular offsets** -- "i mod 5 = 2":

```
{ [i] : exists e : i = 5*e + 2 }
-- Internally: div[0] = floor((i - 2)/5), constraint: i = 5*div[0] + 2
```

**Nested divisions** -- "floor(floor(i/2)/3)":

```
{ [i] -> [j] : j = floor(floor(i/2) / 3) }
-- Internally: div[0] = floor(i/2), div[1] = floor(div[0]/3), constraint: j = div[1]
```

**Stride detection** -- "the set {0, 4, 8, 12, ...} up to N":

```
[N] -> { [i] : exists e : i = 4*e and 0 <= i <= N }
-- ISL detects stride 4, offset 0
```

### Where Div Dimensions Appear

| Context                 | How divs arise                                              |
|-------------------------|-------------------------------------------------------------|
| `isl_aff`              | Affine expressions with `floor(expr/d)` terms               |
| `isl_constraint`       | Constraints with coefficients on div dimensions              |
| `isl_basic_set/map`    | Polyhedral descriptions with existential variables           |
| `isl_local_space`      | The space that tracks div definitions                        |
| Projection / elimination | Eliminating a variable can introduce divs                  |
| Coalescing              | Merging basic sets may require div dimensions for precision  |
| Integer affine hull     | Finding the integer affine subspace introduces divs          |

### The `TFloorDiv` Constructor in Haskell

In the type-level DSL, div dimensions are represented by `TFloorDiv`:

```haskell
type Expr = 'TFloorDiv ('TDim (D 0)) 3   -- floor(dim_0 / 3)
```

In the value-level constraint DSL:

```haskell
FloorDiv (Ix (SetDim 0)) 3   -- floor(i / 3)
```

### Div Dimensions vs. Regular Dimensions

| Property           | Regular Dimensions         | Div Dimensions                    |
|--------------------|----------------------------|-----------------------------------|
| Visible in space   | Yes (part of tuple)        | No (local/hidden)                 |
| Named              | Optionally                 | Never (anonymous)                 |
| Quantification     | Universal (for all)        | Existential (there exists)        |
| Definition         | Free variable              | Defined by floor division         |
| Can depend on divs | N/A                        | Yes (nested divisions)            |
| In ISL strings     | Shown as `e0, e1, ...`    | With `exists` quantifier          |

---

## Level 4: Constraints -- The Atoms of Polyhedra

### `isl_constraint` -- A Single Linear (In)equality

A constraint is a single affine (in)equality over a local space. It is the fundamental building block from which all polyhedra are constructed.

**Equality constraint**: `a0 + a1*x1 + ... + an*xn + b1*p1 + ... + bm*pm + c1*e1 + ... = 0`

**Inequality constraint**: `a0 + a1*x1 + ... + an*xn + b1*p1 + ... + bm*pm + c1*e1 + ... >= 0`

Coefficients are set per dimension type (constant, parameter, set/in/out, div).

### Examples

```
i >= 0           -->  inequality:  i >= 0
i <= N           -->  inequality:  -i + N >= 0
i + j = 2*k     -->  equality:    i + j - 2*k = 0
```

In Haskell, the constraint DSL provides a more natural notation:

```haskell
mkBasicSet @'["N"] @2 $ \[i, j] [n] ->
  [ idx i >=: cst 0          -- i >= 0
  , idx i <=: idx n           -- i <= N
  , idx j >=: cst 0           -- j >= 0
  , idx j <=: idx i           -- j <= i
  ]
```

### Constraint vs. Expression

A **constraint** is a predicate (true/false). An **expression** (`isl_aff`) is a function that returns a value. The two are related:

```
expression:  2*i + j - 3       (returns a number)
constraint:  2*i + j - 3 >= 0  (returns true/false)
```

You can convert between them: `isl_aff_ge_basic_set(aff, aff2)` produces the basic set where `aff >= aff2`.

---

## Level 5: Sets and Maps -- Core Polyhedral Objects

This is the heart of ISL. There is a three-level hierarchy for both sets and maps, from most specific to most general.

### `isl_basic_set` -- A Single Convex Polyhedron

A **basic set** is a conjunction of affine constraints -- a single convex region of integer points.

```
{ [i, j] : 0 <= i <= 10 and 0 <= j <= i }
```

This is the **triangle** with vertices (0,0), (10,0), (10,10). It is convex: any weighted average of two points in the set (rounded to integers) is also in the set.

Limitation: basic sets can only represent **convex** shapes.

```
[N] -> { [i, j] : 0 <= i <= N and 0 <= j <= i }
-- A parametric triangle. N is a symbolic parameter.
```

### `isl_basic_map` -- A Single Convex Relation

A **basic map** is a convex polyhedron over input and output dimensions -- a relation between integer tuples.

```
{ [i, j] -> [i+1, j] : 0 <= i < 10 and 0 <= j <= i }
-- Maps each (i,j) in the triangle to (i+1, j)
```

The domain and range can have different numbers of dimensions:

```
{ [i, j] -> [k] : k = i + j and 0 <= i,j <= 10 }
-- Projects 2D points to their sum
```

### `isl_set` -- Union of Convex Polyhedra

A **set** is a finite union of basic sets, all in the **same space** (same number of dimensions, same tuple name).

```
{ [i] : 0 <= i <= 5 } union { [i] : 10 <= i <= 15 }
-- Non-convex: two disjoint intervals
```

ISL writes this as:

```
{ [i] : (0 <= i <= 5) or (10 <= i <= 15) }
```

Sets can represent any **semi-linear** set (finite union of convex polyhedra). This is the workhorse type for most computations.

### `isl_map` -- Union of Convex Relations

A **map** is a finite union of basic maps in the same space. It can represent non-convex, multi-valued, or partial relations.

```
{ [i] -> [j] : j = i + 1; [i] -> [j] : j = i - 1 }
-- Maps each i to both i+1 and i-1 (multi-valued)
```

Maps are ISL's most versatile type, with the richest set of operations.

### `isl_union_set` -- Sets Across Different Spaces

A **union set** collects sets that live in **different named spaces** (different tuple names or different numbers of dimensions).

```
{ S[i, j] : 0 <= i,j <= 10; T[k] : 0 <= k <= 20 }
```

Here `S` and `T` are tuple identifiers. Statement `S` has a 2D iteration domain; statement `T` has a 1D domain. Union sets are essential for modeling multi-statement programs.

### `isl_union_map` -- Relations Across Different Spaces

A **union map** collects maps from different spaces:

```
{ S[i,j] -> A[i] : ...; T[k] -> B[k,0] : ... }
-- Access relations: statement S reads array A, statement T reads array B
```

Union maps are the standard representation for schedules, dependences, and access relations in polyhedral compilation.

### `isl_point` -- A Single Integer Point

A specific integer point in a set. Extractable via `sample_point` (any point) or `foreach_point` (iterate all, if bounded).

```
point = [3, 7] in space [N] -> { [i, j] }
```

### Hierarchy Summary

```
basic_set  ⊂  set  ⊂  union_set
basic_map  ⊂  map  ⊂  union_map
```

Each level adds expressiveness:
- **basic**: convex, single conjunction
- **non-basic**: non-convex, finite union of basics (same space)
- **union**: multiple named spaces

---

## Level 6: Affine Expressions

### `isl_aff` -- A Single Quasi-Affine Expression

An **affine expression** (or quasi-affine expression) is a function from integer points to rational numbers:

```
f(i, j; N) = (3*i + 2*j - N + 1) / 4
```

More generally, quasi-affine expressions can include floor divisions:

```
f(i; N) = floor((i + N) / 3)
```

An `isl_aff` lives in a **local space** (it can reference div dimensions). It is the building block for all ISL functions.

```
[N] -> { [i, j] -> [(3*i + 2*j - N + 1)] }
```

Operations: arithmetic (+, -, *, /, floor, ceil, mod), pullback (substitute variables), conversion to/from constraints.

### Key Conversions from `isl_aff`

| Operation                  | Result Type | Meaning                              |
|---------------------------|-------------|--------------------------------------|
| `aff_ge_basic_set(a, b)`  | basic_set   | `{ x : a(x) >= b(x) }`             |
| `aff_eq_basic_set(a, b)`  | basic_set   | `{ x : a(x) = b(x) }`              |
| `aff_le_set(a, b)`        | set         | `{ x : a(x) <= b(x) }`             |

---

## Level 7: Multiple and Piecewise -- Type Constructors

ISL builds complex expression types from simple ones using two orthogonal constructors: **multi** (vector-valued) and **piecewise** (domain-partitioned).

### `isl_multi_aff` -- A Vector of Affine Expressions

A **multi_aff** is a tuple of affine expressions sharing the same domain, defining an affine **function** (not relation):

```
{ [i, j] -> [i + j, i - j] }
-- Two output components, each an affine expression of (i, j)
```

This is equivalent to a basic map, but with the guarantee that it is single-valued and total on its domain. Multi-affs are used to represent:

- Loop transformations (skewing, interchange, scaling)
- Array subscript functions
- Coordinate transformations

### `isl_pw_aff` -- Piecewise Quasi-Affine Expression

A **piecewise affine** expression is defined by different affine expressions on disjoint regions of the domain:

```
[N] -> { [i] -> [i] : i < N; [i] -> [N] : i >= N }
-- min(i, N) represented as a piecewise function
```

Each piece is a `(set, aff)` pair. The sets partition (a subset of) the domain. Outside all pieces, the function is undefined.

Piecewise affine expressions arise naturally from:
- `min` and `max` expressions in loop bounds
- Conditional expressions
- Lexicographic optimizations (`lexmin`, `lexmax`)
- Dimension bounds (`dim_min`, `dim_max`)

### `isl_pw_multi_aff` -- Piecewise Vector of Affine Expressions

A vector-valued piecewise function. Each piece maps to a full tuple of affine expressions:

```
{ [i, j] -> [i, j+1] : j < 10; [i, j] -> [i+1, 0] : j = 10 }
-- A "next iteration" function with wrap-around
```

All components share the same domain partition. This is the most common representation for transformations and schedules at the single-statement level.

### `isl_multi_pw_aff` -- Vector of Piecewise Expressions

Each component can have a **different** domain partition:

```
-- Component 0 is piecewise on one partition
-- Component 1 is piecewise on a different partition
```

More general than `pw_multi_aff` (where all components share the same partition).

### `isl_multi_union_pw_aff` -- The Schedule Type

The most general affine function type. A tuple of `union_pw_aff` components, where each component can span **different named spaces**.

```
[{ S[i,j] -> [i]; T[k] -> [k] }, { S[i,j] -> [j]; T[k] -> [0] }]
-- Schedule: time dimension 0 maps S(i,j) to i and T(k) to k
--           time dimension 1 maps S(i,j) to j and T(k) to 0
```

This is the representation used inside schedule tree band nodes.

### The Expression Type Lattice

```
isl_aff
  |                \
  v                 v
isl_pw_aff       isl_multi_aff
  |                |
  v                v
isl_union_pw_aff isl_pw_multi_aff
  |                |
  v                v
  |            isl_multi_pw_aff
  |                |
  v                v
isl_multi_union_pw_aff
```

Each arrow means "is a special case of" (upward = more specific).

### `isl_aff_list` -- Ordered Collection of Affine Expressions

A typed list of `isl_aff` values. Supports: create, add, get, set, drop, concat, size, sort, foreach, map. Used when iterating over components.

---

## Level 8: Quasipolynomials (Counting and Bounding)

### `isl_qpolynomial` -- Polynomial over Integer Points

A **quasipolynomial** extends quasi-affine expressions to allow multiplication of variables:

```
f(i, j) = i^2 + 3*i*j + floor(i/2)
```

A quasipolynomial is a sum of terms, where each term is a product of integer variables, parameters, and floor-division expressions raised to powers.

### `isl_term` -- A Single Monomial

A term in a quasipolynomial: coefficient times a product of powers.

```
3 * i^2 * floor(j/4)^1
```

### `isl_pw_qpolynomial` -- Piecewise Quasipolynomial

Different quasipolynomial expressions on different domain regions. Unlike piecewise affine, the value **outside** all pieces is **zero** (not undefined).

### `isl_qpolynomial_fold` -- Min/Max Reduction

Represents a minimum or maximum over a set of quasipolynomial expressions. Used for bounding:

```
max(i^2, 3*i + 5)  -- an upper bound on some quantity
```

Arises from `isl_pw_qpolynomial_bound`.

### `isl_pw_qpolynomial_fold` -- Piecewise Min/Max

Combines piecewise domains with fold reductions. The result of bounding a piecewise quasipolynomial.

---

## Level 9: Properties and Predicates

### Unary Properties

| Property        | Meaning                                                  | Applies to         |
|-----------------|----------------------------------------------------------|--------------------|
| `is_empty`      | Contains no integer points                               | set, map, pw_*     |
| `is_universe`   | Contains all integer points in its space                 | set                |
| `is_bounded`    | All dimensions are finitely bounded                      | set, basic_set     |
| `is_single_valued` | Each input maps to at most one output                | map                |
| `is_injective`  | Each output comes from at most one input                 | map                |
| `is_bijective`  | Both single-valued and injective                         | map                |
| `is_identity`   | Maps each element only to itself                         | map                |
| `is_wrapping`   | The set's space encodes a wrapped map                    | set                |
| `involves_dims` | Constraints mention the given dimensions                 | set, map, aff      |

### Binary Properties

| Property         | Meaning                                    | Example                     |
|------------------|--------------------------------------------|-----------------------------|
| `is_equal`       | Same mathematical object                   | `{[i]: i>0} = {[i]: i>=1}` |
| `is_subset`      | First contained in second                  | `{[i]: 0<=i<=5} ⊆ {[i]: i>=0}` |
| `is_strict_subset`| Proper subset                             | subset and not equal        |
| `is_disjoint`    | Empty intersection                         | `{[i]: i<0}` and `{[i]: i>0}` |
| `plain_is_equal` | Syntactically identical (fast but incomplete) | may say "no" for equal objects |

---

## Level 10: Unary Operations

### Boolean / Lattice

| Operation     | Description                           | Example                                            |
|---------------|---------------------------------------|-----------------------------------------------------|
| `complement`  | All points NOT in the set             | `complement({[i]: i>=0}) = {[i]: i<0}`             |
| `reverse`     | Swap domain and range of a map        | `reverse({[i]->[i+1]}) = {[i]->[i-1]}`            |

### Extraction

| Operation     | Description                            | Example                                            |
|---------------|----------------------------------------|-----------------------------------------------------|
| `domain`      | Input points of a map                  | `domain({[i]->[j]: j=i+1, i>=0}) = {[i]: i>=0}`   |
| `range`       | Output points of a map                 | `range({[i]->[j]: j=i+1, i>=0}) = {[j]: j>=1}`    |
| `params`      | Parameter-only constraints             | extracts `[N] -> { : N >= 0 }`                     |

### Simplification

| Operation         | Description                                                        |
|-------------------|--------------------------------------------------------------------|
| `coalesce`        | Merge basic components to reduce union size                        |
| `detect_equalities` | Find implicit equalities in the constraint representation        |
| `remove_redundancies` | Drop constraints implied by others                             |
| `gist`            | Simplify assuming a context (remove constraints implied by context) |

**Gist** deserves special attention. Given set `S` and context `C`, `gist(S, C)` returns a simpler set `S'` such that `S' ∩ C = S ∩ C`. The result may be a superset of `S` outside `C`.

```
gist({ [i]: 0 <= i <= 100 }, { [i]: 0 <= i <= 50 }) = { [i]: i <= 100 }
-- The lower bound i>=0 is implied by the context, so it's removed
```

### Hull Operations

| Operation        | Description                                              |
|------------------|----------------------------------------------------------|
| `affine_hull`    | Smallest affine subspace containing the set              |
| `convex_hull`    | Smallest convex superset (exact, may be expensive)       |
| `simple_hull`    | Fast convex over-approximation                           |
| `polyhedral_hull`| Tighter than convex hull for union sets                  |
| `box_hull`       | Over-approximate with a rectangular box                  |

### Lexicographic Optimization

| Operation  | Description                                       |
|------------|---------------------------------------------------|
| `lexmin`   | Lexicographically smallest element(s) of a set    |
| `lexmax`   | Lexicographically largest element(s) of a set     |

```
lexmin({ [i,j] : 0 <= j <= i <= 10 }) = { [0, 0] }
lexmax({ [i,j] : 0 <= j <= i <= 10 }) = { [10, 10] }
```

For maps, `lexmin`/`lexmax` returns the lex-smallest/largest image for each domain point:

```
lexmin({ [i] -> [j] : i <= j <= 10 }) = { [i] -> [i] : i <= 10 }
```

### Dimension Manipulation

| Operation       | Description                                              |
|-----------------|----------------------------------------------------------|
| `project_out`   | Existentially quantify (eliminate) dimensions            |
| `add_dims`      | Add unconstrained dimensions                             |
| `insert_dims`   | Insert dimensions at a position                          |
| `move_dims`     | Move dimensions between types (e.g., set -> param)       |
| `flatten`       | Collapse input+output structure into a single tuple      |

### Structural Transformations

| Operation    | Description                                           |
|-------------|-------------------------------------------------------|
| `wrap`      | Convert map `{ A -> B }` to set `{ [A, B] }`         |
| `unwrap`    | Convert set `{ [A, B] }` to map `{ A -> B }`         |
| `curry`     | `{ (A, B) -> C }` becomes `{ A -> (B -> C) }`        |
| `uncurry`   | `{ A -> (B -> C) }` becomes `{ (A, B) -> C }`        |
| `zip`       | `{ (A -> B) -> (C -> D) }` becomes `{ (A -> C) -> (B -> D) }` |
| `bind`      | Convert set dims to parameters by equating with named params |
| `unbind`    | Convert parameters to set dims                        |

---

## Level 11: Binary Operations

### Set Operations

| Operation    | Description                        | Example                                       |
|--------------|------------------------------------|-----------------------------------------------|
| `intersect`  | Conjunction / AND                  | `{i>=0} ∩ {i<=10} = {0<=i<=10}`              |
| `union`      | Disjunction / OR                   | `{i<=5} ∪ {i>=10}`                           |
| `subtract`   | Set difference                     | `{0<=i<=10} \ {i>=5} = {0<=i<=4}`            |

### Map-Specific Operations

| Operation            | Description                                   |
|----------------------|-----------------------------------------------|
| `intersect_domain`   | Restrict domain to given set                  |
| `intersect_range`    | Restrict range to given set                   |
| `subtract_domain`    | Remove domain points                          |
| `subtract_range`     | Remove range points                           |

### Composition and Application

| Operation | Description                                        | Example                                    |
|-----------|----------------------------------------------------|--------------------------------------------|
| `apply`   | Image of a set under a map                         | `apply({[0,1]}, {[i,j]->[i+j]}) = {[1]}`  |
| `apply_domain` | Compose on the domain side                    |                                            |
| `apply_range`  | Compose on the range side                     |                                            |
| `compose` | Relational composition of two maps                 | If `R: A->B`, `S: B->C`, then `S∘R: A->C` |

### Product Operations

| Operation          | Description                                   |
|--------------------|-----------------------------------------------|
| `flat_product`     | Cartesian product with flattened tuples       |
| `flat_domain_product` | Product on domain side, flatten           |
| `flat_range_product`  | Product on range side, flatten             |
| `product`          | Cartesian product (nested tuples)             |

### Preimage

| Operation                | Description                                       |
|--------------------------|---------------------------------------------------|
| `preimage_multi_aff`     | Preimage of set/map under an affine function      |
| `preimage_pw_multi_aff`  | Preimage under a piecewise affine function        |
| `pullback`               | Precompose a function by another (substitute vars) |

### Lexicographic Comparisons

| Operation    | Description                              |
|-------------|------------------------------------------|
| `lex_lt_set` | `{ [a] -> [b] : a << b }` (lex less-than) |
| `lex_le_set` | `{ [a] -> [b] : a <<= b }`              |
| `lex_gt_set` | `{ [a] -> [b] : a >> b }`               |
| `lex_ge_set` | `{ [a] -> [b] : a >>= b }`              |

---

## Level 12: Transitive Closure

### `transitive_closure` -- Reachability

Given a map `R`, compute `R+` = `R ∪ R∘R ∪ R∘R∘R ∪ ...` (the transitive closure). This is fundamental for:

- Computing reachability in dependence graphs
- Analyzing loop-carried dependences across unbounded iterations
- Proving properties about iterative computations

```
R = { [i] -> [i+1] : 0 <= i }
R+ = { [i] -> [j] : 0 <= i < j }
```

The result may be an **over-approximation** (ISL returns a boolean indicating whether the result is exact).

### `power` -- Parametric Power

Compute `R^k` as a function of parameter `k`:

```
R = { [i] -> [i+2] }
R^k = { [i] -> [i+2k] }
```

### `fixed_power` -- Exact N-th Power

Compute `R^n` for a specific integer `n`.

---

## Level 13: Dependence Analysis

### The Dataflow Problem

Given:
- **Reads** (sink accesses): which data each iteration reads
- **Writes** (source accesses): which data each iteration writes
- **Execution order** (schedule): which iteration runs first

Compute: for each read, which write **most recently** produced the value being read.

### `isl_union_access_info` -- Analysis Input

```
sink:        { S[i,j] -> A[i] }           -- S reads A[i]
must_source: { T[i,j] -> A[i] }           -- T definitely writes A[i]
may_source:  { U[i] -> A[i] }             -- U might write A[i]
kill:        { V[i] -> A[i] }             -- V kills the value at A[i]
schedule:    <schedule tree or map>
```

### `isl_union_flow` -- Analysis Output

```
must_dep:    { T[i',j'] -> S[i,j] : ... } -- definite flow dependences
may_dep:     { U[i'] -> S[i,j] : ... }    -- possible flow dependences
must_no_source: { S[i,j] : ... }          -- reads with no definite source
may_no_source:  { S[i,j] : ... }          -- reads with no possible source
```

---

## Level 14: Scheduling

### The Scheduling Problem

Given iteration domains and dependence relations, find an affine schedule (mapping iterations to time) that:
1. **Respects** all dependences (validity)
2. **Maximizes** parallelism (coincidence)
3. **Maximizes** locality (proximity)

### `isl_schedule_constraints` -- Scheduler Input

| Constraint     | Meaning                                                 |
|---------------|---------------------------------------------------------|
| `domain`       | Iteration domains (union set)                           |
| `validity`     | Must be respected: source before sink                   |
| `coincidence`  | Try to map source and sink to same time (parallelism)   |
| `proximity`    | Try to map source and sink to close times (locality)    |
| `conditional_validity` | Validity only if a condition is met (live ranges) |

### `isl_schedule` -- Scheduler Output

The result is a **schedule tree** (see next section). ISL implements:
- **Pluto-like algorithm**: minimize dependence distances, maximize band depth
- **Feautrier's algorithm**: lexicographically minimize dependence distances (one-to-one)

---

## Level 15: Schedule Trees

A **schedule tree** is a hierarchical representation of how iterations are ordered over time. Each node type serves a specific purpose.

### Node Types

#### `domain` -- Root Node

Declares the set of all domain elements (statement iterations) being scheduled.

```
domain: { S[i,j] : 0 <= i,j <= N; T[k] : 0 <= k <= M }
```

#### `band` -- Time Dimensions

A group of schedule dimensions defined by a `multi_union_pw_aff`. Each dimension maps every statement iteration to a time value.

```
band: [{ S[i,j] -> [i]; T[k] -> [k] }]
  -- schedule dimension 0: S(i,j) executes at time i, T(k) at time k
```

Band properties:
- **coincident**: if true, iterations at the same time can run in parallel
- **permutable**: if true, the dimensions can be freely reordered (tileable)
- **AST loop type**: default, atomic, unroll, or separate

#### `sequence` -- Ordered Execution

Children (each a filter node) execute one after another:

```
sequence:
  filter: { S[i,j] }   -- S runs first
  filter: { T[k] }     -- then T
```

#### `set` -- Unordered Execution

Like sequence, but children may execute in any order (or in parallel).

#### `filter` -- Domain Restriction

Restricts which domain elements reach a subtree:

```
filter: { S[i,j] : i >= 5 }
```

#### `context` -- Parameter Constraints

Declares parameter constraints for the AST generator:

```
context: [N] -> { : N >= 1 }
```

#### `guard` -- Assumed Conditions

Declares conditions the AST generator may assume hold.

#### `expansion` -- Domain Refinement

Maps coarse-grained domain elements to finer-grained ones (with a contraction for the reverse).

#### `extension` -- Extra Domain Elements

Adds additional domain elements to be scheduled (e.g., for inserting copy statements).

#### `mark` -- User Annotation

Attaches user-defined metadata (an `isl_id`) to a subtree. Used for things like GPU mapping annotations.

#### `leaf` -- Terminal

No further ordering imposed.

### Example Schedule Tree

```
domain: { S[i,j] : 0 <= i,j <= N }
  band: [{ S[i,j] -> [i] }] (permutable, coincident)
    band: [{ S[i,j] -> [j] }] (coincident)
      leaf
```

This says: "Execute S(i,j) at time (i, j), with outer loop i and inner loop j, both parallelizable."

### Schedule Tree Transformations

| Operation        | Description                                    |
|------------------|------------------------------------------------|
| `tile`           | Tile a band (strip-mine)                       |
| `split`          | Split a band at a dimension                    |
| `interchange`    | Reorder dimensions within a band               |
| `insert_filter`  | Add a filter node                              |
| `insert_context` | Add parameter constraints                      |
| `insert_mark`    | Attach metadata                                |
| `graft_before/after` | Insert a subtree before/after current      |

---

## Level 16: AST Generation (Code Generation)

### `isl_ast_build` -- The Code Generator

Converts a schedule (tree or map) into an **abstract syntax tree** (AST) suitable for emitting as C, CUDA, OpenCL, etc.

```
schedule tree  -->  isl_ast_build  -->  AST (loops, conditionals, statements)
```

### `isl_ast_node` -- AST Nodes

| Node Type | Description                                    |
|-----------|------------------------------------------------|
| `for`     | A loop: iterator, init, condition, increment   |
| `if`      | Conditional with then-branch, optional else    |
| `block`   | Sequence of statements                         |
| `user`    | A statement instance to execute                |
| `mark`    | User annotation from the schedule tree         |

A `for` node can be **degenerate** (executes exactly once, i.e., the init equals the upper bound).

### `isl_ast_expr` -- AST Expressions

| Expression Type | Description                                          |
|-----------------|------------------------------------------------------|
| `int`           | Integer constant                                     |
| `id`            | Variable/identifier                                  |
| `op`            | Operation: +, -, *, /, fdiv, mod, min, max, cond, and, or, ==, <, <=, ... |

### Example

Given the schedule `{ S[i,j] -> [i,j] : 0 <= i <= N, 0 <= j <= i }`, the AST generator produces:

```c
for (int c0 = 0; c0 <= N; c0 += 1)
  for (int c1 = 0; c1 <= c0; c1 += 1)
    S(c0, c1);
```

---

## Level 17: Parametric Vertex Enumeration

### `isl_vertices` -- Parametric Vertices of a Polyhedron

Computes the vertices of a basic set as quasi-affine expressions in the parameters. For a bounded parametric polyhedron, the vertices move as the parameters change.

```
[N] -> { [i, j] : 0 <= i <= N and 0 <= j <= i }
-- Vertices: (0, 0), (N, 0), (N, N)
-- Each vertex is an isl_multi_aff in terms of N
```

### `isl_vertex` -- A Single Parametric Vertex

Described by:
- A `multi_aff` expression (the vertex coordinates as functions of parameters)
- An activity domain (the parameter values for which this vertex exists)

### `isl_cell` -- A Parameter Space Chamber

The parameter space is decomposed into **chambers**: regions where the same combinatorial set of vertices is active. Each cell has a domain (basic set in parameter space) and a list of active vertices.

---

## Level 18: Stride Information

### `isl_stride_info` -- Stride and Offset of a Dimension

If values along dimension `d` satisfy `d = offset + k * stride` for integer `k`, this captures the stride and offset.

```
{ [i] : exists e : i = 4*e + 1 and 0 <= i <= 100 }
-- stride = 4, offset = 1
-- values: 1, 5, 9, 13, ...
```

### `isl_fixed_box` -- Rectangular Stride Tile

A box (offset + size per dimension) that represents the stride pattern across all dimensions simultaneously. Used for tiling and vectorization analysis.

---

## Level 19: Lists and Associative Arrays

### `isl_*_list` -- Typed Lists

Every major ISL type has a typed list: `isl_val_list`, `isl_set_list`, `isl_map_list`, `isl_basic_set_list`, `isl_aff_list`, etc.

Operations: create, add, get, set, drop, concat, size, sort, foreach, every, map.

### `isl_id_to_ast_expr`, `isl_map_to_basic_set`, ... -- Typed Dictionaries

Associative arrays mapping ISL objects to ISL objects. Used internally for AST generation (mapping statement ids to AST expressions) and for user-defined annotations.

---

## Level 20: Bounds on Quasipolynomials

### `isl_pw_qpolynomial_bound`

Compute upper or lower bounds on piecewise quasipolynomials. Returns a `pw_qpolynomial_fold`. Used for:
- Trip count estimation
- Complexity analysis
- Memory footprint bounding

---

## Summary: ISL Object Hierarchy

```
                    isl_val
                    isl_id
                      |
                  isl_space ---- isl_local_space
                      |                |
                isl_constraint     isl_aff
                      |                |
               isl_basic_set     isl_multi_aff
               isl_basic_map     isl_pw_aff
                      |          isl_pw_multi_aff
                  isl_set        isl_multi_pw_aff
                  isl_map              |
                      |          isl_multi_union_pw_aff
               isl_union_set           |
               isl_union_map     isl_schedule
                      |                |
          isl_union_access_info  isl_schedule_node
                      |                |
                isl_union_flow   isl_ast_build
                                       |
                                 isl_ast_node
                                 isl_ast_expr
```

From scalars to code: ISL provides a complete pipeline from mathematical description of integer sets and relations, through analysis and optimization, to executable code.
