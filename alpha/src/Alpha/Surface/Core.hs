{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
-- 'VarDecl' has only type-level forall'd binders; GHC's unused-foralls
-- warning fires on them even though they are essential for the
-- kind-application form 'VarDecl \@ps \@name \@n \@d \@a' that
-- downstream type families pattern-match on.
{-# OPTIONS_GHC -Wno-unused-foralls #-}

-- | Positional core of the Alpha DSL.
--
-- This module is the typed-AST target of @Alpha.Surface@'s elaboration.
-- Users do not write it directly; they write surface syntax that
-- compiles down to it.  The isl-plugin never looks at the AST itself
-- — it only discharges the type-level constraint obligations
-- ('IslImageSubsetD', 'IslPartitionsD', …) that constructors in this
-- module attach to their indices.
--
-- = Layering
--
-- The module is organised top-down in the semantic stack: we start at
-- the outermost user-visible shape (a closed system) and descend to
-- the type-level plumbing that enforces well-formedness.  Each layer
-- is built from the layers beneath it.
--
--   1. __'System'__ — the top-level closed program: a 'Decls' record
--      (inputs / outputs / locals) paired with an 'EqList' of
--      equations.  The 'DefinesAllExactlyOnce' obligation on the
--      constructor pins down totality: every declared output\/local
--      is defined by exactly one equation, and no equation names an
--      undeclared variable.
--   2. __Equations__ — 'Equation' binds a declared variable to an
--      'Expr' on the variable's declared domain; 'EqList' is the
--      typed snoc-list that threads the defined-names list into the
--      type so that 'DefinesAllExactlyOnce' can read it.
--   3. __Surface declarations__ — 'Decl', 'DeclList', and the
--      'Decls' record that partitions declarations into the three
--      roles (input\/output\/local).  These are value-level mirrors
--      of the kind-level 'VarDecl's — no runtime payload, all shape
--      information flows through the phantom types.
--   4. __Expressions__ — 'Expr' (the core GADT) and 'Branches' (the
--      snoc-list used by 'Case').  Every constructor's obligations
--      are either structural Haskell equalities or plugin-discharged
--      classes from "Isl.TypeLevel.Reflection" — 'IslImageSubsetD'
--      (array-access bounds), 'IslPartitionsD' ('Case' totality),
--      and friends.
--   5. __Variable declarations__ — 'VarDecl' is the kind-level
--      description of a declared variable (name + dims + domain tag
--      + base type).  The projections 'DeclName' \/ 'DeclDims' \/
--      'DeclDomTag' \/ 'DeclType', the 'Lookup' family, and the
--      'ReplaceDecl' family all manipulate lists of these.
--   6. __Type-level plumbing__ — list append, 'CountName',
--      'RemoveName', and the 'DefinesAllExactlyOnce' class that
--      implements the totality obligation used by 'System'.
--
-- = Trust base
--
-- Zero unsafe coercions in this module.  The replace\/lookup axioms
-- ('withReplaceDecl', 'replaceDeclList', 'withDefinesAllReplace',
-- 'withReplaceDeclConcat') are isolated in "Alpha.Core.Lemmas".
-- All other obligations are discharged by GHC or by the isl-plugin
-- via the @Isl.TypeLevel.Reflection@ wrapper classes.
module Alpha.Surface.Core
  ( -- * Systems
    System(System)
    -- * Equations
  , Equation(..)
  , EqList(..)
  , eqListNames
    -- * Surface-level declarations
  , Decl(..)
  , DeclList(..)
  , Decls(..)
    -- * Expressions
  , Expr(..)
    -- * Case branches
  , Branches(..)
    -- * Variable declarations
  , VarDecl(..)
  , DeclName, DeclDims, DeclDomTag, DeclType
  , Lookup
  , ReplaceDecl, ReplaceDeclStep
    -- * Type-level supporting families
  , type (++)
  , DefinesAllExactlyOnce(..)
  , DefinesAllExactlyOnceStep(..)
  , RemoveName
  , CountName
    -- * Re-exports for interpretation
  , KnownConstraints
    -- * Re-exports for Expr operations
  , BinOp(..), UnaryOp(..), ReduceOp(..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits
  ( CmpSymbol, ErrorMessage(..), KnownNat, KnownSymbol, Nat, Symbol
  , TypeError, symbolVal, type (+) )

import Isl.Typed.Params (KnownSymbols)
import Isl.TypeLevel.Constraint (TConstraint, IslNonEmpty, LiftPctxN)
import Isl.TypeLevel.Reflection
  ( Append
  , DomTag(..)
  , EffectiveDomTag
  , IslImageEqualD
  , IslImageSubsetD
  , IslPartitionsD
  , IslSubsetD
  , KnownDom
  , LitPrepend
  , MapLitPrepend
  )
import Isl.TypeLevel.Sing (KnownConstraints)
import Alpha.Codegen.COp (BinOp(..), UnaryOp(..), ReduceOp(..))
import Alpha.Scalar (AlphaScalar)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Systems
-- ═══════════════════════════════════════════════════════════════════════

-- | A complete Alpha system.
--
-- The 'System' constructor combines a 'Decls' record (declarations
-- partitioned by role) with an 'EqList' (equations).  The
-- 'DefinesAllExactlyOnce' constraint enforces that the equation list
-- defines exactly the union of declared outputs and locals, each one
-- exactly once, in any order.
type System
  :: forall (ps :: [Symbol])
  -> [TConstraint ps 0]
  -> [VarDecl ps] -> [VarDecl ps] -> [VarDecl ps] -> Type
data System ps pctx inputs outputs locals where
  System
    :: forall ps pctx inputs outputs locals defined.
       ( KnownSymbols ps
       , IslNonEmpty ps 0 pctx
       , KnownConstraints ps 0 pctx
       , DefinesAllExactlyOnce (outputs ++ locals) defined )
    => Decls  ps inputs outputs locals
    -> EqList ps pctx (inputs ++ outputs ++ locals) defined
    -> System ps pctx inputs outputs locals


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Equations
-- ═══════════════════════════════════════════════════════════════════════

-- | An equation binds a declared variable to an expression on the
-- variable's declared domain.  The equation's name is carried in the
-- *type* (the @name@ parameter) so that the snoc-list 'EqList' can
-- thread it into the type-level @defined@ list at construction time.
--
-- The 'Defines' constructor's equality constraints force the body's
-- type parameters to match the declared variable: dimension count,
-- domain tag, and base type all come from @Lookup name decls@.  An
-- attempt to define a variable with a body of the wrong shape is a
-- Haskell type error before the plugin even runs.
type Equation
  :: forall (ps :: [Symbol])
  -> [TConstraint ps 0] -> [VarDecl ps] -> Symbol -> Type
data Equation ps pctx decls name where
  Defines :: forall name ps pctx decls (decl :: VarDecl ps).
             ( decl ~ Lookup name decls
             , KnownSymbol name
             , KnownNat (DeclDims decl)
             , KnownDom ps (DeclDims decl) (DeclDomTag decl) )
          => Proxy name
          -> Expr ps pctx decls (DeclDims decl) (DeclDomTag decl) (DeclType decl)
          -> Equation ps pctx decls name

-- | A typed snoc-list of equations.  The @defined@ parameter is the
-- type-level list of names defined by this list, in order of insertion.
-- The ':&' constructor extends the list with the equation's name,
-- which is carried in the equation's type so that the extension is
-- structurally forced.
type EqList :: forall (ps :: [Symbol])
            -> [TConstraint ps 0] -> [VarDecl ps] -> [Symbol] -> Type
data EqList ps pctx decls defined where
  EqNil :: EqList ps pctx decls '[]
  (:&)
    :: KnownSymbol name
    => Equation ps pctx decls name
    -> EqList ps pctx decls rest
    -> EqList ps pctx decls (name ': rest)
infixr 5 :&

-- | Extract the names of all equations in an 'EqList', in declaration
-- order.  The names are carried structurally in each 'Defines'
-- constructor via @Proxy name@; this helper materialises them as a
-- plain @[String]@ for use by downstream passes (codegen, compile,
-- interpret) that walk the equation list.
eqListNames :: forall ps pctx decls defined. EqList ps pctx decls defined -> [String]
eqListNames EqNil = []
eqListNames (Defines (Proxy :: Proxy name) _ :& rest) =
  symbolVal (Proxy @name) : eqListNames rest


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Surface declarations
-- ═══════════════════════════════════════════════════════════════════════

-- | A system's declarations partitioned into three roles.  Each field
-- is a typed snoc-list; the role of a declaration is determined by
-- which field it lives in (a 'Decl' value carries no role tag itself).
--
-- Constructed by record syntax in surface programs:
--
-- @
-- 'Decls' { 'dInputs'  = input  \@\"A\" ... :> input \@\"B\" ... :> 'Nil'
--        , 'dOutputs' = output \@\"C\" ... :> 'Nil'
--        , 'dLocals'  = 'Nil'
--        }
-- @
type Decls
  :: forall (ps :: [Symbol])
  -> [VarDecl ps] -> [VarDecl ps] -> [VarDecl ps] -> Type
data Decls ps inputs outputs locals where
  Decls
    :: { dInputs  :: DeclList ps inputs
       , dOutputs :: DeclList ps outputs
       , dLocals  :: DeclList ps locals
       } -> Decls ps inputs outputs locals

-- | A typed snoc-list of declarations.  The @decls@ parameter is the
-- list of 'VarDecl' kinds in declaration order.  Built with ':>' and
-- 'Nil'; consumed by 'Decls' below.
type DeclList :: forall (ps :: [Symbol]) -> [VarDecl ps] -> Type
data DeclList ps decls where
  Nil  :: DeclList ps '[]
  (:>) :: Decl ps d -> DeclList ps ds -> DeclList ps (d ': ds)
infixr 5 :>

-- | A single declaration at the value level.  The 'Decl' value carries
-- the @VarDecl@ as a phantom type; there is no runtime payload because
-- everything the system needs is in the type.  Constructed by
-- 'Alpha.Surface.input' / 'output' / 'local'.
type Decl :: forall (ps :: [Symbol]) -> VarDecl ps -> Type
data Decl ps d where
  MkDecl :: ( KnownSymbol (DeclName d)
            , KnownNat (DeclDims d)
            , KnownDom ps (DeclDims d) (DeclDomTag d)
            , AlphaScalar (DeclType d)
            ) => Decl ps d


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Expressions
-- ═══════════════════════════════════════════════════════════════════════

-- | An Alpha expression living in parameter space @ps@, with declaration
-- environment @decls@, dimension count @n@, domain tag @d@, producing
-- values of type @a@.
--
-- Every constructor's obligations either are structural (GHC checks
-- them) or fire as plugin obligations through the 'IslSubsetD' /
-- 'IslImageSubsetD' / 'IslRangeOfD' / 'IslCoversD' classes from
-- "Isl.TypeLevel.Reflection".
type Expr :: forall (ps :: [Symbol])
         -> [TConstraint ps 0]
         -> [VarDecl ps] -> forall (n :: Nat)
         -> DomTag ps n -> Type -> Type
data Expr ps pctx decls n d a where

  -- | Variable reference.  Looks up the named variable in @decls@ and
  -- produces an expression on the variable's *declared domain*.  The
  -- usual surrounding 'Dep' node then reindexes that into a body
  -- domain (and bears the array-access bounds check via
  -- 'IslImageSubsetD').
  Var :: forall (name :: Symbol) (ps :: [Symbol])
                (pctx :: [TConstraint ps 0])
                (decls :: [VarDecl ps]) (decl :: VarDecl ps).
         ( decl ~ Lookup name decls
         , KnownDom ps (DeclDims decl) (DeclDomTag decl)
         , KnownSymbol name )
      => Proxy name
      -> Expr ps pctx decls (DeclDims decl) (DeclDomTag decl) (DeclType decl)

  -- | Domain-polymorphic literal.  GHC unifies @d@ with the checking
  -- context.
  --
  -- The 'AlphaScalar' constraint captures the type's 'ConstBridge' (and
  -- rest of its scalar metadata) at construction time; pattern-matching
  -- 'Const' in a renderer brings the dictionary back into scope, so the
  -- C rendering does not need to consult a separate 'ScalarDesc' map
  -- via a typed hatch.
  Const :: AlphaScalar a => a -> Expr ps pctx decls n d a

  -- | Pointwise binary operation.  Both operands and the result share
  -- the same domain @d@ and the same value type @a@.  Domain matching
  -- is a Haskell-level type equality, not a plugin obligation.
  --
  -- The 'BinOp' tag makes the operation renderable to C; the
  -- interpreter evaluates it at 'Double' via 'evalBinOp'.
  Pw :: BinOp
     -> Expr ps pctx decls n d a
     -> Expr ps pctx decls n d a
     -> Expr ps pctx decls n d a

  -- | Pointwise unary operation (sqrt, negate, abs, …).
  --
  -- The 'UnaryOp' tag makes the operation renderable to C; the
  -- interpreter evaluates it at 'Double' via 'evalUnaryOp'.
  PMap :: UnaryOp
       -> Expr ps pctx decls n d a
       -> Expr ps pctx decls n d a

  -- | Dependence: apply an affine map to reindex from an inner
  -- variable space (@no@-dim) into an outer body space (@ni@-dim).
  --
  -- The constructor fuses the System's @pctx@ into every sub-obligation
  -- by lifting it to the relevant dimension count: the map-space
  -- constraint list is prefixed with @LiftPctxN (ni+no) pctx@, and
  -- the two Literal-shaped DomTag phantoms are wrapped via 'LitPrepend'
  -- with the corresponding @LiftPctxN ni pctx@ / @LiftPctxN no pctx@.
  -- The plugin then sees pctx as part of the map-space constraint
  -- list rather than via a separate givens channel.
  Dep :: forall ps pctx decls (ni :: Nat) (no :: Nat)
              (mapCs :: [TConstraint ps (ni + no)])
              (dOuter :: DomTag ps ni) (dInner :: DomTag ps no) a.
         ( KnownNat ni, KnownNat no
         , KnownDom ps ni dOuter
         , KnownDom ps no dInner
         -- Bare mapCs dict (for the interpreter's affine evaluation;
         -- the plugin-obligation variant below lifts pctx in).
         , KnownConstraints ps (ni + no) mapCs
         , IslImageSubsetD ps ni no
             (Append (LiftPctxN (ni + no) pctx) mapCs)
             (LitPrepend (LiftPctxN ni pctx) dOuter)
             (LitPrepend (LiftPctxN no pctx) dInner) )
      => Proxy mapCs
      -> Expr ps pctx decls no dInner a
      -> Expr ps pctx decls ni dOuter a

  -- | Reduction: project out one or more dimensions via an
  -- associative operation.
  --
  -- The reduction's body lives in a larger space @nBody@/@dBody@; the
  -- projection map (encoded as @projCs@) sends body points to result
  -- points, and the *image* of @dBody@ under the projection /equals/
  -- the result domain @d@ ('IslImageEqualD'); transforms that wish to
  -- narrow the result domain must wrap in 'Restrict' rather than
  -- rewrite the phantom directly.
  --
  -- The 'ReduceOp' tag determines the accumulation operation:
  -- 'ReduceSum' → addition (identity 0), 'ReduceProd' → multiplication
  -- (identity 1), etc.  The interpreter and codegen use this directly.
  Reduce :: forall ps pctx decls (n :: Nat) (nBody :: Nat)
                  (projCs :: [TConstraint ps (nBody + n)])
                  (d :: DomTag ps n) (dBody :: DomTag ps nBody) a.
            ( KnownNat n, KnownNat nBody
            , KnownDom ps n d
            , KnownDom ps nBody dBody
            , KnownConstraints ps (nBody + n) projCs
            , IslImageEqualD ps nBody n
                (Append (LiftPctxN (nBody + n) pctx) projCs)
                (LitPrepend (LiftPctxN nBody pctx) dBody)
                (LitPrepend (LiftPctxN n pctx) d) )
         => ReduceOp
         -> Proxy projCs
         -> Expr ps pctx decls nBody dBody a
         -> Expr ps pctx decls n     d     a

  -- | Case: a list of branches whose domains cover @d@ and are
  -- pairwise disjoint within @d@.  Each point in the ambient is
  -- defined by exactly one branch — a branching recurrence denotes
  -- a function, and a function can't assign two different values
  -- to the same point.  Enforced at compile time by 'IslPartitionsD'.
  --
  -- === Lowering strategy (fan-out to polyhedral statements)
  --
  -- A top-level @Case branches@ on an equation's RHS is fanned out by
  -- 'Alpha.Transform.NormalizeCases' + 'Alpha.Lower' into /N polyhedral
  -- statements/ — one per branch, each with domain @d_i ∩ amb@ — all
  -- writing to the same logical array.  ISL's scanner then emits
  -- peeled/split loop nests automatically.  The partition witness
  -- @'IslPartitionsD' ps n d branchDoms@ is what makes the fan-out
  -- sound: disjoint write-map ranges, covering union of domains.
  --
  -- Per-statement 'Alpha.Codegen.ReduceInfo' means branches may freely
  -- mix reduction shapes (including a non-reducing branch next to a
  -- reducing one, or two branches reducing over different dims).
  --
  -- === Transform polarity (IMPORTANT for 'Alpha.Transform.*' authors)
  --
  -- A 'Case' partitions its ambient @d@.  A transform that narrows @d@
  -- to @d'@ cannot in general re-partition the branches: the witnesses
  -- @branchDoms@ are fixed, and their intersection with @d'@ may no
  -- longer cover @d'@ (coverage is a property of @d@, not @d'@).
  -- As with 'Reduce', a constructor-dispatched phantom rewrite is
  -- unsound; the universal move is to wrap via 'Restrict'.
  -- See 'Alpha.Transform.Restrict.restrict'.
  --
  -- === Nested Case (inside 'Reduce' body or 'Dep' target)
  --
  -- Only a /top-level/ 'Case' fans out to separate statements.  A
  -- 'Case' inside a 'Reduce' body lives in @nBody@, not @n@ — and
  -- lifting it out would require introducing auxiliary equations for
  -- per-branch partial reductions combined by an outer fold (the
  -- sub-reduction images overlap in the result domain, which is
  -- exactly what reduction /is/).  That distribution is out of scope
  -- for 'Alpha.Transform.NormalizeCases'; nested Cases render via the
  -- existing ternary path in 'Alpha.Codegen.ExprRender' — correct,
  -- just not optimally loop-nested.
  Case :: forall ps pctx decls (n :: Nat) (d :: DomTag ps n)
                 (branchDoms :: [DomTag ps n]) a.
          ( KnownNat n
          , KnownDom ps n d
          , IslPartitionsD ps n
              (LitPrepend (LiftPctxN n pctx) d)
              (MapLitPrepend (LiftPctxN n pctx) branchDoms) )
       => Branches ps pctx decls n d branchDoms a
       -> Expr ps pctx decls n d a

  -- | Explicit subsumption: narrow an expression's declared domain
  -- from @dInner@ down to any @dOuter@ with @dOuter ⊆ dInner@.  The
  -- 'IslSubsetD' obligation is the type-level inclusion certificate;
  -- the elaborator cashes it into a Core 'Subset' token.
  Restrict :: forall ps pctx decls n dInner dOuter a.
              ( KnownNat n
              , KnownDom ps n dOuter, KnownDom ps n dInner
              , IslSubsetD ps n
                  (LitPrepend (LiftPctxN n pctx) dOuter)
                  (LitPrepend (LiftPctxN n pctx) dInner) )
           => Expr ps pctx decls n dInner a
           -> Expr ps pctx decls n dOuter a


-- | A list of case branches.  Each entry carries its own declared
-- domain tag @d@ and a body; the body's phantom is the /effective/
-- domain @d ∩ amb@ (see 'EffectiveDomTag') so that array-access
-- obligations inside the body run against @d ∩ amb@ rather than
-- the raw declared @d@.  This captures the fact that a branch guard
-- is implicitly intersected with the ambient at evaluation time,
-- which in turn means "over-wide" declared branches are
-- semantically harmless — their clip is exactly the well-formed
-- branch would have been.
--
-- The list is a snoc-list at the type level via the @branchDoms@
-- parameter so that 'IslPartitionsD' (on the enclosing 'Case') can
-- read the collection of all branch domains.
type Branches :: forall (ps :: [Symbol])
             -> [TConstraint ps 0]
             -> [VarDecl ps] -> forall (n :: Nat)
             -> DomTag ps n -> [DomTag ps n] -> Type -> Type
data Branches ps pctx decls n amb branchDoms a where
  BNil :: Branches ps pctx decls n amb '[] a
  BCons :: ( KnownDom ps n d
           , KnownDom ps n (EffectiveDomTag d amb) )
        => Proxy d
        -> Expr ps pctx decls n (EffectiveDomTag d amb) a
        -> Branches ps pctx decls n amb ds a
        -> Branches ps pctx decls n amb (d ': ds) a


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Variable declarations
-- ═══════════════════════════════════════════════════════════════════════

-- | A single variable declaration at the kind level.  All four fields
-- live entirely at the kind level — the constructor has no value-level
-- arguments.  This is necessary because the declared domain @d :: DomTag
-- ps n@ has a kind that depends on @n@; mixing value-level and
-- type-level fields confuses GHC's kind inference.
--
-- Inhabitants of @[VarDecl ps]@ at the kind level are written using
-- explicit type applications:
--
-- @
-- '['VarDecl' \@ps \@\"A\" \@2 \@('Literal' aCs) \@Double, ...]
-- @
type VarDecl :: forall (ps :: [Symbol]) -> Type
data VarDecl ps where
  VarDecl
    :: forall (ps :: [Symbol]) (name :: Symbol) (n :: Nat)
              (d :: DomTag ps n) (a :: Type).
       VarDecl ps

-- | Look up a variable's declaration by name.  Stuck (with a friendly
-- 'TypeError') if the name is not present.
type family Lookup (name :: Symbol) (decls :: [VarDecl ps]) :: VarDecl ps where
  Lookup name ('VarDecl @ps @name @n @d @a ': _) = 'VarDecl @ps @name @n @d @a
  Lookup name (_ ': rest)                        = Lookup name rest
  Lookup name '[] =
    TypeError ('Text "Alpha: unknown variable " ':<>: 'ShowType name)

-- | Projections out of a 'VarDecl'.  These pattern-match on the
-- type-application form of the promoted constructor.
type family DeclName (d :: VarDecl ps) :: Symbol where
  DeclName ('VarDecl @_ @name @_ @_ @_) = name

type family DeclDims (d :: VarDecl ps) :: Nat where
  DeclDims ('VarDecl @_ @_ @n @_ @_) = n

type family DeclDomTag (d :: VarDecl ps) :: DomTag ps (DeclDims d) where
  DeclDomTag ('VarDecl @_ @_ @_ @dt @_) = dt

type family DeclType (d :: VarDecl ps) :: Type where
  DeclType ('VarDecl @_ @_ @_ @_ @a) = a


-- | Substitute the 'VarDecl' named @name@ in @xs@ with @new@.
-- Preserves order and all other entries.
type family ReplaceDecl (name :: Symbol) (new :: VarDecl ps)
                        (xs :: [VarDecl ps]) :: [VarDecl ps] where
  ReplaceDecl _    _   '[]         = '[]
  ReplaceDecl name new (x ': rest) =
    ReplaceDeclStep (CmpSymbol name (DeclName x)) name new x rest

-- | Step function for 'ReplaceDecl'.
type family ReplaceDeclStep (cmp :: Ordering) (name :: Symbol)
                            (new :: VarDecl ps) (x :: VarDecl ps)
                            (rest :: [VarDecl ps]) :: [VarDecl ps] where
  ReplaceDeclStep 'EQ _    new _ rest = new ': rest
  ReplaceDeclStep _   name new x rest = x ': ReplaceDecl name new rest


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Type-level supporting families
-- ═══════════════════════════════════════════════════════════════════════

-- | Type-level list append.
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++

-- | Count the number of declarations in @decls@ whose name equals
-- @name@.  Used by 'DefinesAllExactlyOnce' to detect missing or
-- duplicate definitions.
type family CountName (name :: Symbol) (decls :: [VarDecl ps]) :: Nat where
  CountName _    '[]                                            = 0
  CountName name ('VarDecl @_ @dn @_ @_ @_ ': rest)             =
    CountNameStep (CmpSymbol name dn) name rest

type family CountNameStep
              (cmp :: Ordering) (name :: Symbol) (rest :: [VarDecl ps]) :: Nat where
  CountNameStep 'EQ name rest = 1 + CountName name rest
  CountNameStep _   name rest = CountName name rest

-- | Remove the first declaration whose name equals @name@ from a
-- declaration list.  Used by 'DefinesAllExactlyOnce' to walk through
-- the declaration set as the equation list is consumed.
type family RemoveName (name :: Symbol) (decls :: [VarDecl ps]) :: [VarDecl ps] where
  RemoveName _    '[] = '[]
  RemoveName name (('VarDecl @ps @dn @n @d @a) ': rest) =
    RemoveNameStep (CmpSymbol name dn) name ('VarDecl @ps @dn @n @d @a) rest

type family RemoveNameStep
              (cmp :: Ordering) (name :: Symbol)
              (h :: VarDecl ps) (rest :: [VarDecl ps]) :: [VarDecl ps] where
  RemoveNameStep 'EQ _    _ rest                = rest
  RemoveNameStep _   name h rest                = h ': RemoveName name rest

-- | The system-level totality obligation.  Holds iff every variable
-- declared in @needed@ is defined exactly once by some equation in
-- @defined@, and no equation defines a variable not in @needed@.
--
-- Phrased operationally: walk @defined@ left to right; for each name,
-- assert that it appears exactly once in the still-pending @needed@
-- list, and remove it.  When @defined@ is exhausted, @needed@ must be
-- empty (no missing definitions).
--
-- Encoded as a class with a nullary method 'definesAllExactlyOnceEv'
-- and 'TypeError' in instance contexts so that under
-- @-fdefer-type-errors@ the trap fires at runtime when the dictionary
-- field is demanded.  A @Constraint@-returning type family would lose
-- the trap in an erased coercion.
class DefinesAllExactlyOnce
        (needed :: [VarDecl ps]) (defined :: [Symbol]) where
  definesAllExactlyOnceEv :: ()

-- Success case: needed and defined are both empty.
instance DefinesAllExactlyOnce ('[] :: [VarDecl ps]) ('[] :: [Symbol]) where
  definesAllExactlyOnceEv = ()

-- Failure case: needed is non-empty but defined is empty — some
-- variables are missing equations.  The instance context carries a
-- custom 'TypeError' message (fires at compile time — same message
-- as before); the method body calls 'error' so that when the module
-- is built with @-fdefer-type-errors@, the runtime trap fires when
-- 'definesAllExactlyOnceEv' is demanded.
instance {-# OVERLAPPING #-}
  ( TypeError ('Text "Alpha: missing equation definitions for: "
               ':<>: 'ShowType (NamesOf (n ': ns)))
  ) => DefinesAllExactlyOnce ((n ': ns) :: [VarDecl ps]) ('[] :: [Symbol]) where
  definesAllExactlyOnceEv =
    error "Alpha: missing equation definitions \
          \(runtime trap; see the deferred TypeError for details)"

-- Recursive case: defined is non-empty.  Dispatch on the count of
-- @name@ in @needed@ via 'DefinesAllExactlyOnceStep'.
instance DefinesAllExactlyOnceStep
           (CountName name needed) name needed rest
  => DefinesAllExactlyOnce (needed :: [VarDecl ps]) (name ': rest) where
  definesAllExactlyOnceEv =
    definesAllExactlyOnceStepEv @ps @(CountName name needed) @name @needed @rest

class DefinesAllExactlyOnceStep
        (count :: Nat) (name :: Symbol)
        (needed :: [VarDecl ps]) (rest :: [Symbol]) where
  definesAllExactlyOnceStepEv :: ()

-- Count 0: equation defines a variable not in the declaration list.
-- TypeError in context for the compile-time message; 'error' in the
-- body so defer-type-errors fires at runtime when
-- 'definesAllExactlyOnceEv' is demanded.
instance {-# OVERLAPPING #-}
  ( TypeError ('Text "Alpha: equation defines variable "
               ':<>: 'ShowType name
               ':<>: 'Text " which is not declared as an output or local")
  ) => DefinesAllExactlyOnceStep 0 name (needed :: [VarDecl ps]) rest where
  definesAllExactlyOnceStepEv =
    error "Alpha: equation defines an undeclared variable \
          \(runtime trap; see the deferred TypeError for details)"

instance {-# OVERLAPPING #-}
  DefinesAllExactlyOnce (RemoveName name needed) rest
  => DefinesAllExactlyOnceStep 1 name (needed :: [VarDecl ps]) rest where
  definesAllExactlyOnceStepEv =
    definesAllExactlyOnceEv @ps @(RemoveName name needed) @rest

-- Count > 1: variable is defined by more than one equation.
-- TypeError in context for the compile-time message; 'error' in the
-- body so defer-type-errors fires at runtime when
-- 'definesAllExactlyOnceEv' is demanded.
instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "Alpha: variable "
               ':<>: 'ShowType name
               ':<>: 'Text " is defined by more than one equation")
  ) => DefinesAllExactlyOnceStep count name (needed :: [VarDecl ps]) rest where
  definesAllExactlyOnceStepEv =
    error "Alpha: variable is defined by more than one equation \
          \(runtime trap; see the deferred TypeError for details)"

-- | Helper: extract the names from a list of 'VarDecl's, for use in
-- 'TypeError' messages.
type family NamesOf (decls :: [VarDecl ps]) :: [Symbol] where
  NamesOf '[] = '[]
  NamesOf ('VarDecl @_ @name @_ @_ @_ ': rest) = name ': NamesOf rest
