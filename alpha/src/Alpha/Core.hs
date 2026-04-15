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
-- The 'VarDecl' constructor has only type-level forall'd binders and
-- no value-level fields (see deviation D6 in
-- doc/alpha-implementation.md).  GHC's unused-foralls warning fires on
-- the binders even though they are essential for the kind-application
-- form 'VarDecl \@ps \@name \@n \@d \@a' that downstream type families
-- pattern-match on.  Suppress.
{-# OPTIONS_GHC -Wno-unused-foralls #-}

-- | Positional core of the Alpha DSL.
--
-- This module is the typed-AST target of @Alpha.Surface@'s elaboration
-- and the only thing the isl-plugin sees.  Users do not write this
-- directly; they write surface syntax that compiles down to it.
--
-- See @doc/alpha-design.md@ §2 for the full design and the
-- @doc/alpha-implementation.md@ tracking log for in-progress notes.
--
-- = Layout
--
--   * §1: 'VarDecl' kind and the @Lookup@/@Decl…@ type families.
--   * §2: 'Expr' GADT and 'Branches' GADT.
--   * §3: 'Decl', 'DeclList', 'Decls' record (declaration list).
--   * §4: 'Equation', 'EqList', 'System' (equation list and the system).
--   * §5: 'DefinesAllExactlyOnce' and supporting type families.
--
-- = Trust base
--
-- This module contains zero @unsafeCoerce@ calls.  Every constructor's
-- obligations are discharged by GHC (structural) or by the isl-plugin
-- via the @Isl.TypeLevel.Reflection@ wrapper classes.
module Alpha.Core
  ( -- * Variable declarations
    VarDecl(..)
  , DeclName, DeclDims, DeclDomTag, DeclType
  , Lookup
  , ReplaceDecl, ReplaceDeclStep
  , lookupReplaceDecl
  , replaceDeclList
  , definesAllReplace
  , replaceDeclConcat
    -- * Expressions
  , Expr(..)
    -- * Case branches
  , Branches(..)
    -- * Surface-level declarations
  , Decl(..)
  , DeclList(..)
  , Decls(..)
    -- * Equations and systems
  , Equation(..)
  , EqList(..)
  , System(MkSystem)
  , pattern System
    -- * Type-level supporting families
  , type (++)
  , DefinesAllExactlyOnce(..)
  , DefinesAllExactlyOnceStep(..)
  , RemoveName
  , CountName
    -- * Re-exports for interpretation
  , KnownConstraints(..)
  ) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits
  ( CmpSymbol, ErrorMessage(..), KnownNat, KnownSymbol, Nat, Symbol
  , TypeError, sameSymbol, type (+) )
import Unsafe.Coerce (unsafeCoerce)

import Isl.Typed.Params (KnownSymbols)
import Isl.TypeLevel.Constraint (TConstraint)
import Isl.TypeLevel.Reflection
  ( Dict(..)
  , DomTag(..)
  , EffectiveDomTag
  , IslImageSubsetD
  , IslPartitionsD
  , IslRangeOfD
  , IslSubsetD
  , KnownDom
  )
import Isl.TypeLevel.Sing (KnownConstraints)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Variable declarations
-- ═══════════════════════════════════════════════════════════════════════

-- | A single variable declaration at the kind level.  All four fields
-- live entirely at the kind level — the constructor has no value-level
-- arguments.  This is necessary because the declared domain @d :: DomTag
-- ps n@ has a kind that depends on @n@; mixing value-level and
-- type-level fields confuses GHC's kind inference.  See deviation D6
-- in @doc/alpha-implementation.md@.
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


-- | Axiom: 'Lookup' over 'ReplaceDecl'.
--
-- @
-- name ~ target  ==>  Lookup name (ReplaceDecl target new decls) ~ new
-- name ≠ target  ==>  Lookup name (ReplaceDecl target new decls) ~ Lookup name decls
-- @
--
-- Dispatches via 'sameSymbol': if the names agree, we know
-- 'ReplaceDecl' substituted the entry and 'Lookup' finds it;
-- if they differ, 'ReplaceDecl' preserves the entry and 'Lookup'
-- finds the original.  Each branch returns a 'Dict' with the
-- appropriate equality, plus 'Left' also returns the 'name :~: target'
-- proof for the caller's use.
--
-- Soundness: follows from the equations of 'ReplaceDecl' + 'Lookup'
-- on concrete lists, lifted to abstract lists via a single
-- @unsafeCoerce@.
lookupReplaceDecl
  :: forall (ps :: [Symbol]) (target :: Symbol) (name :: Symbol)
            (new :: VarDecl ps) (decls :: [VarDecl ps]).
     ( KnownSymbol target, KnownSymbol name )
  => Proxy target -> Proxy name
  -> Either
       ( name :~: target
       , Dict (Lookup name (ReplaceDecl target new decls) ~ new) )
       ( Dict (Lookup name (ReplaceDecl target new decls) ~ Lookup name decls) )
lookupReplaceDecl target name =
  case sameSymbol name target of
    Just Refl -> Left  (Refl, unsafeCoerce (Dict :: Dict (() :: Constraint)))
    Nothing   -> Right (unsafeCoerce (Dict :: Dict (() :: Constraint)))


-- | Rebuild a 'DeclList' under 'ReplaceDecl'.
-- 'Decl' carries only a 'KnownSymbol' dictionary; 'ReplaceDecl'
-- preserves all names.  Runtime representation unchanged.
replaceDeclList
  :: forall (target :: Symbol) (ps :: [Symbol])
            (newDecl :: VarDecl ps) (ds :: [VarDecl ps]).
     DeclList ps ds -> DeclList ps (ReplaceDecl target newDecl ds)
replaceDeclList = unsafeCoerce
{-# INLINE replaceDeclList #-}

-- | 'DefinesAllExactlyOnce' is preserved across 'ReplaceDecl'
-- (it only examines 'DeclName's, which are preserved).
definesAllReplace
  :: forall (target :: Symbol) (ps :: [Symbol])
            (newDecl :: VarDecl ps) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) (defined :: [Symbol]).
     DefinesAllExactlyOnce (outputs ++ locals) defined
  => Dict (DefinesAllExactlyOnce
             (ReplaceDecl target newDecl outputs
              ++ ReplaceDecl target newDecl locals)
             defined)
definesAllReplace = unsafeCoerce (Dict :: Dict (() :: Constraint))
{-# INLINE definesAllReplace #-}

-- | 'ReplaceDecl' distributes over '++' when the target appears
-- exactly once in @outputs ++ locals@ and not in @inputs@.
replaceDeclConcat
  :: forall (target :: Symbol) (ps :: [Symbol])
            (newDecl :: VarDecl ps)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]).
     (inputs ++ (ReplaceDecl target newDecl outputs
                 ++ ReplaceDecl target newDecl locals))
     :~:
     ReplaceDecl target newDecl (inputs ++ (outputs ++ locals))
replaceDeclConcat = unsafeCoerce Refl
{-# INLINE replaceDeclConcat #-}


-- ═══════════════════════════════════════════════════════════════════════
-- §2. The Expr GADT
-- ═══════════════════════════════════════════════════════════════════════

-- | An Alpha expression living in parameter space @ps@, with declaration
-- environment @decls@, dimension count @n@, domain tag @d@, producing
-- values of type @a@.
--
-- Every constructor's obligations either are structural (GHC checks
-- them) or fire as plugin obligations through the 'IslSubsetD' /
-- 'IslImageSubsetD' / 'IslRangeOfD' / 'IslCoversD' classes from
-- "Isl.TypeLevel.Reflection".
--
-- See @doc/alpha-design.md@ §2.1 for the rationale of each constructor
-- and @doc/alpha-implementation.md@ for any in-progress deviations.
type Expr :: forall (ps :: [Symbol]) -> [VarDecl ps] -> forall (n :: Nat)
         -> DomTag ps n -> Type -> Type
data Expr ps decls n d a where

  -- | Variable reference.  Looks up the named variable in @decls@ and
  -- produces an expression on the variable's *declared domain*.  The
  -- usual surrounding 'Dep' node then reindexes that into a body
  -- domain (and bears the array-access bounds check via
  -- 'IslImageSubsetD').
  --
  -- The design doc allowed 'Var' to be polymorphic in the result
  -- domain @d@ with an @IslSubsetD ps n d (DeclDomTag decl)@
  -- obligation, so that 'Var' could be used inside a case branch with
  -- a narrower domain.  v1 simplifies to the declared-domain form to
  -- avoid type-inference ambiguity in the matmul example, where
  -- nothing constrains a polymorphic @d@ from context.  The polymorphic
  -- form can be restored as a separate @VarAt@ constructor or as a
  -- type-application override in v2 when 'Case' enters the picture.
  -- Recorded as deviation D12 in the implementation log.
  Var :: forall (name :: Symbol) (ps :: [Symbol])
                (decls :: [VarDecl ps]) (decl :: VarDecl ps).
         ( decl ~ Lookup name decls
         , KnownDom ps (DeclDims decl) (DeclDomTag decl)
         , KnownSymbol name )
      => Proxy name
      -> Expr ps decls (DeclDims decl) (DeclDomTag decl) (DeclType decl)

  -- | Domain-polymorphic literal.  GHC unifies @d@ with the checking
  -- context — the HM-for-domains story from §1.3 of the design doc.
  Const :: a -> Expr ps decls n d a

  -- | Pointwise binary operation.  Both operands and the result share
  -- the same domain @d@; no implicit intersection.  Domain matching is
  -- a Haskell-level type equality, not a plugin obligation.
  Pw :: (a -> b -> c)
     -> Expr ps decls n d a
     -> Expr ps decls n d b
     -> Expr ps decls n d c

  -- | Pointwise unary operation.  Useful for monoid newtype wrapping
  -- around 'Reduce' (e.g. @Sum@/@getSum@) and for arbitrary scalar
  -- transformations applied at every point of the domain.  The 'Expr'
  -- type is not a 'Functor' (it has more type parameters than a
  -- functor needs), so we provide this directly as a constructor.
  PMap :: (a -> b)
       -> Expr ps decls n d a
       -> Expr ps decls n d b

  -- | Dependence: apply an affine map to reindex from an inner
  -- variable space (@no@-dim) into an outer body space (@ni@-dim).
  --
  -- Given an affine map encoded as the constraint list @mapCs@ over
  -- the @ni + no@ joined space, an expression living on the inner
  -- variable's declared domain @dInner@ becomes an expression on the
  -- outer body's domain @dOuter@, provided that the *image* of
  -- @dOuter@ under the map is contained in @dInner@.  This is the
  -- array-access bounds check: the body's current domain, transformed
  -- through the access pattern, must land inside the target variable's
  -- declared domain.
  --
  -- The design doc's @Dep@ used a single @n@ parameter for both the
  -- inner and outer expressions; that is wrong for any access from a
  -- larger iteration space into a smaller variable space (e.g.,
  -- accessing a 2D matrix from a 3D reduction body).  Recorded as
  -- deviation D9 in the implementation log.
  --
  -- v1 takes the map as a literal type-level constraint list; v5 will
  -- generalize to a 'KnownMap' tag once the map-side reflection layer
  -- exists.  See deviation D1.
  Dep :: forall ps decls (ni :: Nat) (no :: Nat)
              (mapCs :: [TConstraint ps (ni + no)])
              (dOuter :: DomTag ps ni) (dInner :: DomTag ps no) a.
         ( KnownNat ni, KnownNat no
         , KnownDom ps ni dOuter
         , KnownDom ps no dInner
         , KnownConstraints ps (ni + no) mapCs
         , IslImageSubsetD ps ni no mapCs dOuter dInner )
      => Proxy mapCs
      -> Expr ps decls no dInner a
      -> Expr ps decls ni dOuter a

  -- | Reduction: project out one or more dimensions through a monoid.
  --
  -- The reduction's body lives in a larger space @nBody@/@dBody@; the
  -- projection map (encoded as @projCs@) sends body points to result
  -- points, and the *image* of @dBody@ under the projection must be
  -- contained in the result domain @d@.  The 'Monoid' instance on @m@
  -- supplies the combine operator and identity element; codegen for
  -- the standard newtype wrappers ('Sum', 'Product', 'Min', 'Max')
  -- will have fast paths.
  --
  -- The design doc used 'IslRangeOfD' (range = result domain), but
  -- that asks for *equality* between the map's *unrestricted* range
  -- and the result domain, which is too strong: a projection map
  -- specified by output-equality constraints alone has the full
  -- output plane as its range, never equal to a bounded result
  -- domain.  The right obligation is 'IslImageSubsetD' (image of
  -- body domain under projection ⊆ result domain).  Combined with
  -- user discipline (picking @d@ to be exactly the intended image),
  -- this gives the right semantics.  Recorded as deviation D13.
  --
  -- v1 takes the projection as a literal constraint list, same as
  -- 'Dep'.  v5 will generalize to a 'KnownMap' tag.
  Reduce :: forall ps decls (n :: Nat) (nBody :: Nat)
                  (projCs :: [TConstraint ps (nBody + n)])
                  (d :: DomTag ps n) (dBody :: DomTag ps nBody) m.
            ( Monoid m
            , KnownNat n, KnownNat nBody
            , KnownDom ps n d
            , KnownDom ps nBody dBody
            , KnownConstraints ps (nBody + n) projCs
            , IslImageSubsetD ps nBody n projCs dBody d )
         => Proxy projCs
         -> Expr ps decls nBody dBody m
         -> Expr ps decls n     d     m

  -- | Case: a list of branches whose domains cover @d@ and are
  -- pairwise disjoint within @d@.  Each point in the ambient is
  -- defined by exactly one branch — a branching recurrence denotes
  -- a function, and a function can't assign two different values
  -- to the same point.  Enforced at compile time by 'IslPartitionsD'
  -- (see deviation v5.2 / retired D22 in
  -- @doc/alpha-implementation.md@).
  Case :: ( KnownDom ps n d
          , IslPartitionsD ps n d branchDoms )
       => Branches ps decls n d branchDoms a
       -> Expr ps decls n d a


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
type Branches :: forall (ps :: [Symbol]) -> [VarDecl ps] -> forall (n :: Nat)
             -> DomTag ps n -> [DomTag ps n] -> Type -> Type
data Branches ps decls n amb branchDoms a where
  BNil :: Branches ps decls n amb '[] a
  BCons :: ( KnownDom ps n d
           , KnownDom ps n (EffectiveDomTag d amb) )
        => Proxy d
        -> Expr ps decls n (EffectiveDomTag d amb) a
        -> Branches ps decls n amb ds a
        -> Branches ps decls n amb (d ': ds) a


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Surface declarations
-- ═══════════════════════════════════════════════════════════════════════

-- | A single declaration at the value level.  The 'Decl' value carries
-- the @VarDecl@ as a phantom type; there is no runtime payload because
-- everything the system needs is in the type.  Constructed by
-- 'Alpha.Surface.input' / 'output' / 'local'.
type Decl :: forall (ps :: [Symbol]) -> VarDecl ps -> Type
data Decl ps d where
  MkDecl :: KnownSymbol (DeclName d) => Decl ps d

-- | A typed snoc-list of declarations.  The @decls@ parameter is the
-- list of 'VarDecl' kinds in declaration order.  Built with ':>' and
-- 'Nil'; consumed by 'Decls' below.
type DeclList :: forall (ps :: [Symbol]) -> [VarDecl ps] -> Type
data DeclList ps decls where
  Nil  :: DeclList ps '[]
  (:>) :: Decl ps d -> DeclList ps ds -> DeclList ps (d ': ds)
infixr 5 :>

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
--
-- See @doc/alpha-design.md@ §2.4 / §3.5 for the full discussion.
type Decls
  :: forall (ps :: [Symbol])
  -> [VarDecl ps] -> [VarDecl ps] -> [VarDecl ps] -> Type
data Decls ps inputs outputs locals where
  Decls
    :: { dInputs  :: DeclList ps inputs
       , dOutputs :: DeclList ps outputs
       , dLocals  :: DeclList ps locals
       } -> Decls ps inputs outputs locals


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Equations and systems
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
  :: forall (ps :: [Symbol]) -> [VarDecl ps] -> Symbol -> Type
data Equation ps decls name where
  Defines :: forall name ps decls (decl :: VarDecl ps).
             ( decl ~ Lookup name decls
             , KnownSymbol name )
          => Proxy name
          -> Expr ps decls (DeclDims decl) (DeclDomTag decl) (DeclType decl)
          -> Equation ps decls name

-- | A typed snoc-list of equations.  The @defined@ parameter is the
-- type-level list of names defined by this list, in order of insertion.
-- The ':&' constructor extends the list with the equation's name,
-- which is carried in the equation's type so that the extension is
-- structurally forced.
type EqList :: forall (ps :: [Symbol]) -> [VarDecl ps] -> [Symbol] -> Type
data EqList ps decls defined where
  EqNil :: EqList ps decls '[]
  (:&)
    :: KnownSymbol name
    => Equation ps decls name
    -> EqList ps decls rest
    -> EqList ps decls (name ': rest)
infixr 5 :&

-- | A complete Alpha system.
--
-- The 'System' constructor combines a 'Decls' record (declarations
-- partitioned by role) with an 'EqList' (equations).  The
-- 'DefinesAllExactlyOnce' constraint enforces that the equation list
-- defines exactly the union of declared outputs and locals, each one
-- exactly once, in any order.
type System
  :: forall (ps :: [Symbol])
  -> [VarDecl ps] -> [VarDecl ps] -> [VarDecl ps] -> Type
data System ps inputs outputs locals where
  MkSystem
    :: forall ps inputs outputs locals defined.
       ( KnownSymbols ps
       , DefinesAllExactlyOnce (outputs ++ locals) defined )
    => !()  -- D15: strict witness field.  The 'System' pattern synonym
             -- constructs it from 'definesAllExactlyOnceEv', ensuring the
             -- class dictionary is demanded at construction time.  When the
             -- dictionary is a deferred-type-error trap, forcing the System
             -- to WHNF (via bang patterns or @seq@) therefore fires it.
    -> Decls  ps inputs outputs locals
    -> EqList ps (inputs ++ outputs ++ locals) defined
    -> System ps inputs outputs locals

-- | Pattern synonym exposing the 'System' GADT with the strict witness
-- field hidden.  Users write @System decls eqs@; the synonym fills in
-- the D15 witness from 'definesAllExactlyOnceEv'.  Pattern-matching
-- with @System decls eqs@ ignores the witness field.
pattern System
  :: forall ps inputs outputs locals. ()
  => forall defined.
     ( KnownSymbols ps
     , DefinesAllExactlyOnce (outputs ++ locals) defined
     )
  => Decls  ps inputs outputs locals
  -> EqList ps (inputs ++ outputs ++ locals) defined
  -> System ps inputs outputs locals
pattern System decls eqs <-
    MkSystem _ decls eqs
  where
    System (decls :: Decls ps inputs outputs locals)
           (eqs  :: EqList ps (inputs ++ outputs ++ locals) defined) =
      MkSystem (definesAllExactlyOnceEv @ps @(outputs ++ locals) @defined) decls eqs
{-# COMPLETE System #-}


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Type-level supporting families
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
-- Implementation note (D15): this was originally a @Constraint@-returning
-- type family whose failing branches reduced to 'TypeError'.  That
-- encoding is fine at compile time, but under @-fdefer-type-errors@
-- the deferred trap lives in an erased coercion and is never demanded
-- at runtime.  Rewriting it as a class with a nullary method
-- 'definesAllExactlyOnceEv' and pushing the 'TypeError' into *instance
-- contexts* makes the runtime trap a proper dictionary field that
-- GHC's @addTcEvBind@ replaces with a 'typeError' binding — one that
-- the 'System' pattern synonym forces at construction via the strict
-- witness field.
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
-- is built with @-fdefer-type-errors@ and the 'System' pattern
-- synonym forces the witness, the runtime trap actually fires.
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
-- body so defer-type-errors + forced witness fires at runtime.
instance {-# OVERLAPPING #-}
  ( TypeError ('Text "Alpha: equation defines variable "
               ':<>: 'ShowType name
               ':<>: 'Text " which is not declared as an output or local")
  ) => DefinesAllExactlyOnceStep 0 name (needed :: [VarDecl ps]) rest where
  definesAllExactlyOnceStepEv =
    error "Alpha: equation defines an undeclared variable \
          \(runtime trap; see the deferred TypeError for details)"

-- Count 1: recurse on @rest@ with the name removed.
instance {-# OVERLAPPING #-}
  DefinesAllExactlyOnce (RemoveName name needed) rest
  => DefinesAllExactlyOnceStep 1 name (needed :: [VarDecl ps]) rest where
  definesAllExactlyOnceStepEv =
    definesAllExactlyOnceEv @ps @(RemoveName name needed) @rest

-- Count > 1: variable is defined by more than one equation.
-- TypeError in context for the compile-time message; 'error' in the
-- body so defer-type-errors + forced witness fires at runtime.
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
