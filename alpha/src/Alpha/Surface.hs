{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Suppress -Wunused-foralls on GADTs with type-level-only binders.
{-# OPTIONS_GHC -Wno-unused-foralls #-}

-- | Named-binder surface layer for the Alpha DSL.
--
-- This module is what users write; it elaborates to @Alpha.Core@.
-- See @doc/alpha-design.md@ §3 for the design.
--
-- = Overview
--
-- Users write bodies using named dimension variables (@#i@, @#j@, @#k@
-- via @OverloadedLabels@) instead of positional @D 0@, @D 1@, @D 2@.
-- Each combinator (@at@, @sumOver@, @caseB@, @when_@, @def@) constructs
-- the corresponding @Alpha.Core@ GADT node directly, with type families
-- compiling the named surface to positional type-level constraint lists.
--
-- All plugin obligations dispatch through the literal route at compile
-- time.  Zero @unsafeCoerce@ in this module.
module Alpha.Surface
  ( -- * Index expressions
    IxE(..), IxExpr(..)
  , par, lit, nlit
    -- * IxExpr operators
  , (+.), (-.)
    -- * Heterogeneous index lists
  , IxList(..), (|:|), iNil
  , ix1, ix2, ix3, ix4
    -- * Domain expressions
  , DomE(..), DomExpr(..)
  , (.>=.), (.<=.), (.==.), (/\)
  , between, range0
    -- * Body-level combinators
  , Body(..)
  , (.+.), (.-.), (.*.), (./.)
  , litB, mapB, pwB
  , at
  , reduceOver
  , sumOver
    -- * Case branches
  , SBranches(..), when_, caseB
    -- * Declarations
  , input, output, local
    -- * Equations and systems
  , def
  , system
    -- * Type families (for advanced use / type signatures)
  , Resolve, CompileIx, CompileIxList, CompileDom
  , IxListLength
  , IdentityHeadIds
  , DomsToLitList, AppendCs
    -- * Re-exports from Alpha.Core (for system construction)
  , module Alpha.Core
  ) where

import Data.Kind (Type)
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits
  ( ErrorMessage(..), KnownNat, KnownSymbol, Nat, Symbol
  , TypeError, type (+) )

import Isl.Typed.Params (KnownSymbols, Length)
import Isl.TypeLevel.Constraint
  ( IslMultiAffToMap, TConstraint
  , type (>=.), type (<=.), type (==.) )
import Isl.TypeLevel.Expr
  ( D, Elem, P, TExpr(..), Z(..) )
import Isl.TypeLevel.Reflection
  ( DomTag(..), EffectiveDomTag
  , IslImageSubsetD, IslPartitionsD
  , KnownDom )

import Alpha.Core


-- ═══════════════════════════════════════════════════════════════════════
-- §A. Promoted description ADTs
-- ═══════════════════════════════════════════════════════════════════════

-- | Promoted description kind for index-expression shapes.
-- The 'IxExpr' GADT is indexed by this kind, and the 'CompileIx' type
-- family walks it to produce a 'TExpr'.
data IxE
  = IxN Symbol          -- ^ Named scope reference (@#i@)
  | IxP Symbol          -- ^ Parameter reference (@par \@\"N\"@)
  | IxC Z               -- ^ Integer constant (signed via 'Z')
  | IxA IxE IxE         -- ^ Addition
  | IxM Z   IxE         -- ^ Scalar multiplication (@k * e@)

-- | Promoted description kind for domain-expression shapes.
-- Each constructor compiles to a conjunction of @TConstraint@s via
-- 'CompileDom'.
data DomE
  = DomGe  IxE IxE          -- ^ @a >= b@
  | DomLe  IxE IxE          -- ^ @a <= b@
  | DomEq  IxE IxE          -- ^ @a == b@
  | DomAnd DomE DomE        -- ^ conjunction


-- ═══════════════════════════════════════════════════════════════════════
-- §B. Type families
-- ═══════════════════════════════════════════════════════════════════════

-- | Resolve a dimension name to its 0-indexed position in the scope.
-- Stuck with a friendly 'TypeError' if the name is missing.
type family Resolve (s :: Symbol) (scope :: [Symbol]) :: Nat where
  Resolve s '[] = TypeError
    ('Text "Alpha.Surface: dimension name "
       ':<>: 'ShowType s
       ':<>: 'Text " is not in the current scope")
  Resolve s (s ': _)  = 0
  Resolve s (_ ': xs) = 1 + Resolve s xs

-- | Constraint alias: @s@ must appear in @scope@.
type InScope s scope = Elem s scope ~ 'True

-- | Compile a single 'IxE' description to a 'TExpr'.
type family CompileIx (ps :: [Symbol]) (scope :: [Symbol]) (n :: Nat)
                      (e :: IxE) :: TExpr ps n where
  CompileIx ps scope n ('IxN s)   = 'TDim   (D (Resolve s scope))
  CompileIx ps scope n ('IxP p)   = 'TParam (P p)
  CompileIx ps scope n ('IxC k)   = 'TConst k
  CompileIx ps scope n ('IxA a b) = 'TAdd (CompileIx ps scope n a)
                                          (CompileIx ps scope n b)
  CompileIx ps scope n ('IxM k a) = 'TMul k (CompileIx ps scope n a)

-- | Compile a list of 'IxE' descriptions (the output expressions of an
-- access map).
type family CompileIxList (ps :: [Symbol]) (scope :: [Symbol]) (n :: Nat)
                          (es :: [IxE]) :: [TExpr ps n] where
  CompileIxList _  _     _ '[]       = '[]
  CompileIxList ps scope n (e ': es) =
    CompileIx ps scope n e ': CompileIxList ps scope n es

-- | Length of an 'IxE' description list (for arity matching).
type family IxListLength (es :: [IxE]) :: Nat where
  IxListLength '[]       = 0
  IxListLength (_ ': es) = 1 + IxListLength es

-- | Compile a 'DomE' tree to a flat list of 'TConstraint's.
-- Takes the dimension count @n@ explicitly to avoid GHC's
-- non-injective-type-family ambiguity on @Length scope@.
type family CompileDom (ps :: [Symbol]) (scope :: [Symbol]) (n :: Nat)
                       (d :: DomE) :: [TConstraint ps n] where
  CompileDom ps scope n ('DomGe a b) =
    '[ CompileIx ps scope n a >=. CompileIx ps scope n b ]
  CompileDom ps scope n ('DomLe a b) =
    '[ CompileIx ps scope n a <=. CompileIx ps scope n b ]
  CompileDom ps scope n ('DomEq a b) =
    '[ CompileIx ps scope n a ==. CompileIx ps scope n b ]
  CompileDom ps scope n ('DomAnd a b) =
    AppendCs (CompileDom ps scope n a) (CompileDom ps scope n b)

-- | Append two constraint lists.
type family AppendCs (xs :: [TConstraint ps n]) (ys :: [TConstraint ps n])
                  :: [TConstraint ps n] where
  AppendCs '[]       ys = ys
  AppendCs (x ': xs) ys = x ': AppendCs xs ys

-- | Build the identity projection @[D 0, D 1, …, D (Length outerScope - 1)]@.
-- Used by 'reduceOver' to project out the appended reduction variable.
type family IdentityHeadIds (ps :: [Symbol]) (nBody :: Nat) (pos :: Nat)
                            (outerScope :: [Symbol])
                         :: [TExpr ps nBody] where
  IdentityHeadIds _  _     _   '[]       = '[]
  IdentityHeadIds ps nBody pos (_ ': xs) =
    'TDim (D pos) ': IdentityHeadIds ps nBody (1 + pos) xs

-- | Map a list of domain descriptions to their compiled 'Literal' tags.
-- Takes @n@ explicitly (same as 'CompileDom') to avoid Length non-injectivity.
type family DomsToLitList (ps :: [Symbol]) (scope :: [Symbol]) (n :: Nat)
                          (ds :: [DomE]) :: [DomTag ps n] where
  DomsToLitList _  _     _ '[]       = '[]
  DomsToLitList ps scope n (d ': ds) =
    'Literal (CompileDom ps scope n d) ': DomsToLitList ps scope n ds


-- ═══════════════════════════════════════════════════════════════════════
-- §C. Value-level GADTs
-- ═══════════════════════════════════════════════════════════════════════

-- | An index expression in a named scope, indexed at the kind level by
-- its 'IxE' description.  The kind index is what lets 'CompileIxList'
-- walk the tree at the type level.
type IxExpr :: [Symbol] -> IxE -> Type
data IxExpr scope ix where
  IxNamed :: forall s scope. (KnownSymbol s, InScope s scope)
          => Proxy s -> IxExpr scope ('IxN s)
  IxParam :: forall p scope. KnownSymbol p
          => Proxy p -> IxExpr scope ('IxP p)
  IxConst :: forall (k :: Z) scope. IxExpr scope ('IxC k)
  IxPlus  :: IxExpr scope a -> IxExpr scope b
          -> IxExpr scope ('IxA a b)
  IxMulC  :: forall (k :: Z) scope a.
             IxExpr scope a -> IxExpr scope ('IxM k a)

-- | Heterogeneous list of index expressions, preserving kind-level structure.
-- Use 'ix1'..'ix4' for common arities, or '(|:|)' / 'iNil' for generic
-- construction.
type IxList :: [Symbol] -> [IxE] -> Type
data IxList scope ixs where
  INil  :: IxList scope '[]
  ICons :: IxExpr scope ix -> IxList scope ixs
        -> IxList scope (ix ': ixs)

-- | A domain expression in a named scope, indexed at the kind level by
-- its 'DomE' description.
type DomExpr :: [Symbol] -> DomE -> Type
data DomExpr scope dom where
  DGe  :: IxExpr scope a -> IxExpr scope b
       -> DomExpr scope ('DomGe a b)
  DLe  :: IxExpr scope a -> IxExpr scope b
       -> DomExpr scope ('DomLe a b)
  DEq  :: IxExpr scope a -> IxExpr scope b
       -> DomExpr scope ('DomEq a b)
  DAnd :: DomExpr scope d1 -> DomExpr scope d2
       -> DomExpr scope ('DomAnd d1 d2)


-- ═══════════════════════════════════════════════════════════════════════
-- §D. Smart constructors and instances
-- ═══════════════════════════════════════════════════════════════════════

-- | @#i@ in scope @'[\"i\", \"j\"]@ resolves to dimension 0.
-- Requires @OverloadedLabels@.
instance (KnownSymbol s, InScope s scope, ix ~ 'IxN s)
      => IsLabel s (IxExpr scope ix) where
  fromLabel = IxNamed (Proxy @s)

-- | Reference a parameter by name.  @par \@\"N\"@ ≈ type-level @'TParam (P \"N\")@.
par :: forall p scope. KnownSymbol p => IxExpr scope ('IxP p)
par = IxParam (Proxy @p)

-- | Non-negative integer literal.  @lit \@3@ ≈ type-level @'TConst ('Pos 3)@.
lit :: forall (k :: Nat) scope. IxExpr scope ('IxC ('Pos k))
lit = IxConst

-- | Negative integer literal.  @nlit \@2@ ≈ type-level @'TConst ('Neg 2)@.
nlit :: forall (k :: Nat) scope. IxExpr scope ('IxC ('Neg k))
nlit = IxConst

-- | Index addition.
(+.) :: IxExpr scope a -> IxExpr scope b -> IxExpr scope ('IxA a b)
(+.) = IxPlus
infixl 6 +.

-- | Index subtraction.  @a -. b@ ≡ @a +. (−1 * b)@.
(-.) :: IxExpr scope a -> IxExpr scope b
     -> IxExpr scope ('IxA a ('IxM ('Neg 1) b))
a -. b = IxPlus a (IxMulC @('Neg 1) b)
infixl 6 -.

-- | Domain constraint: @a >= b@.
(.>=.) :: IxExpr scope a -> IxExpr scope b -> DomExpr scope ('DomGe a b)
(.>=.) = DGe
infix 4 .>=.

-- | Domain constraint: @a <= b@.
(.<=.) :: IxExpr scope a -> IxExpr scope b -> DomExpr scope ('DomLe a b)
(.<=.) = DLe
infix 4 .<=.

-- | Domain constraint: @a == b@.
(.==.) :: IxExpr scope a -> IxExpr scope b -> DomExpr scope ('DomEq a b)
(.==.) = DEq
infix 4 .==.

-- | Domain conjunction.
(/\) :: DomExpr scope d1 -> DomExpr scope d2 -> DomExpr scope ('DomAnd d1 d2)
(/\) = DAnd
infixr 3 /\

-- | @lo ≤ x ≤ hi@.
between :: IxExpr scope lo -> IxExpr scope hi -> IxExpr scope x
        -> DomExpr scope ('DomAnd ('DomGe x lo) ('DomLe x hi))
between lo hi x = x .>=. lo /\ x .<=. hi

-- | @0 ≤ x ≤ p − 1@ where @p@ is a named parameter.
range0 :: forall p scope x. KnownSymbol p
       => IxExpr scope x
       -> DomExpr scope ('DomAnd ('DomGe x ('IxC ('Pos 0)))
                                 ('DomLe x ('IxA ('IxP p) ('IxM ('Neg 1) ('IxC ('Pos 1))))))
range0 x = between (lit @0) (par @p -. lit @1) x

-- | Empty index list.
iNil :: IxList scope '[]
iNil = INil

-- | Heterogeneous cons for index lists.
(|:|) :: IxExpr scope ix -> IxList scope ixs -> IxList scope (ix ': ixs)
(|:|) = ICons
infixr 5 |:|

-- | 1-element index list.
ix1 :: IxExpr scope a
    -> IxList scope '[a]
ix1 a = ICons a INil

-- | 2-element index list.
ix2 :: IxExpr scope a -> IxExpr scope b
    -> IxList scope '[a, b]
ix2 a b = ICons a (ICons b INil)

-- | 3-element index list.
ix3 :: IxExpr scope a -> IxExpr scope b -> IxExpr scope c
    -> IxList scope '[a, b, c]
ix3 a b c = ICons a (ICons b (ICons c INil))

-- | 4-element index list.
ix4 :: IxExpr scope a -> IxExpr scope b -> IxExpr scope c -> IxExpr scope d
    -> IxList scope '[a, b, c, d]
ix4 a b c d = ICons a (ICons b (ICons c (ICons d INil)))


-- ═══════════════════════════════════════════════════════════════════════
-- §E. Body-level combinators
-- ═══════════════════════════════════════════════════════════════════════

-- | A body expression in a named scope.  Newtype over 'Expr' that
-- carries @scope@ structurally so GHC can propagate it (unlike a
-- type alias through 'Length', which is non-injective).
--
-- The explicit @n@ parameter decouples the dimension count from
-- @Length scope@ so that 'def' can bridge @Length scope ~ DeclDims decl@
-- without a kind-level equality check that GHC can't perform.
-- Combinators constrain @n ~ Length scope@ via their own signatures.
newtype Body ps decls (scope :: [Symbol]) (n :: Nat) d a =
  Body { unBody :: Expr ps decls n d a }

-- | Pointwise addition.
(.+.) :: Num a => Body ps decls scope n d a -> Body ps decls scope n d a
      -> Body ps decls scope n d a
a .+. b = Body (Pw (+) (unBody a) (unBody b))
infixl 6 .+.

-- | Pointwise subtraction.
(.-.) :: Num a => Body ps decls scope n d a -> Body ps decls scope n d a
      -> Body ps decls scope n d a
a .-. b = Body (Pw (-) (unBody a) (unBody b))
infixl 6 .-.

-- | Pointwise multiplication.
(.*.) :: Num a => Body ps decls scope n d a -> Body ps decls scope n d a
      -> Body ps decls scope n d a
a .*. b = Body (Pw (*) (unBody a) (unBody b))
infixl 7 .*.

-- | Pointwise division.
(./.) :: Fractional a => Body ps decls scope n d a -> Body ps decls scope n d a
      -> Body ps decls scope n d a
a ./. b = Body (Pw (/) (unBody a) (unBody b))
infixl 7 ./.

-- | Body-level literal (wraps 'Const').
litB :: a -> Body ps decls scope n d a
litB = Body . Const

-- | Lift a unary function over a body (wraps 'PMap').
mapB :: (a -> b) -> Body ps decls scope n d a -> Body ps decls scope n d b
mapB f = Body . PMap f . unBody

-- | Pointwise binary operation (wraps 'Pw').
pwB :: (a -> a -> a) -> Body ps decls scope n d a -> Body ps decls scope n d a
    -> Body ps decls scope n d a
pwB f a b = Body (Pw f (unBody a) (unBody b))

-- | Array access.  @at \@\"A\" (ix2 #i #k)@ elaborates to a
-- @Dep (Proxy \@(IslMultiAffToMap …)) (Var (Proxy \@\"A\"))@.
--
-- The access map is computed at the type level from the index list's
-- kind-level descriptions via 'CompileIxList', and the plugin's
-- 'IslImageSubsetD' obligation dispatches through the literal route.
at :: forall (name :: Symbol) {ps} {decls} {scope :: [Symbol]}
            {ixs :: [IxE]} {decl :: VarDecl ps} {bodyDom}.
      ( decl ~ Lookup name decls
      , KnownSymbol name
      , KnownNat (Length scope), KnownNat (DeclDims decl)
      , IxListLength ixs ~ DeclDims decl
      , KnownDom ps (DeclDims decl) (DeclDomTag decl)
      , KnownDom ps (Length scope) bodyDom
      , KnownConstraints ps (Length scope + DeclDims decl)
          (IslMultiAffToMap ps (Length scope) (DeclDims decl)
             (CompileIxList ps scope (Length scope) ixs))
      , IslImageSubsetD ps (Length scope) (DeclDims decl)
          (IslMultiAffToMap ps (Length scope) (DeclDims decl)
             (CompileIxList ps scope (Length scope) ixs))
          bodyDom
          (DeclDomTag decl)
      )
   => IxList scope ixs
   -> Body ps decls scope (Length scope) bodyDom (DeclType decl)
at _ = Body $ Dep (Proxy :: Proxy
                     (IslMultiAffToMap ps (Length scope) (DeclDims decl)
                        (CompileIxList ps scope (Length scope) ixs)))
                  (Var (Proxy @name))

-- | Monoid reduction over a named dimension.
--
-- The reduction variable is /appended/ to the outer scope so that the
-- body's Core representation matches the existing hand-written examples
-- byte-for-byte (dims 0..n-1 are the outer scope, dim n is the
-- reduction variable).
--
-- @bodyDomE@ is the body's domain, spelled out as a 'DomExpr' in the
-- extended scope.  v7 keeps this explicit; v8 may auto-compute it from
-- the outer domain and the reduction variable's range.
reduceOver
  :: forall (k :: Symbol) {outerScope :: [Symbol]} {bodyDomE :: DomE}
            {ps} {decls} {dOuter} {m}.
     ( KnownSymbol k
     , Elem k outerScope ~ 'False
     , Monoid m
     , KnownNat (Length outerScope)
     , KnownNat (Length (outerScope ++ '[k]))
     , KnownDom ps (Length outerScope) dOuter
     , KnownDom ps (Length (outerScope ++ '[k]))
                   ('Literal (CompileDom ps (outerScope ++ '[k]) (Length (outerScope ++ '[k])) bodyDomE))
     , KnownConstraints ps (Length (outerScope ++ '[k]) + Length outerScope)
         (IslMultiAffToMap ps (Length (outerScope ++ '[k])) (Length outerScope)
            (IdentityHeadIds ps (Length (outerScope ++ '[k])) 0 outerScope))
     , IslImageSubsetD ps (Length (outerScope ++ '[k])) (Length outerScope)
         (IslMultiAffToMap ps (Length (outerScope ++ '[k])) (Length outerScope)
            (IdentityHeadIds ps (Length (outerScope ++ '[k])) 0 outerScope))
         ('Literal (CompileDom ps (outerScope ++ '[k]) (Length (outerScope ++ '[k])) bodyDomE))
         dOuter
     )
  => DomExpr (outerScope ++ '[k]) bodyDomE
  -> Body ps decls (outerScope ++ '[k]) (Length (outerScope ++ '[k]))
          ('Literal (CompileDom ps (outerScope ++ '[k]) (Length (outerScope ++ '[k])) bodyDomE)) m
  -> Body ps decls outerScope (Length outerScope) dOuter m
reduceOver _ (Body body) =
  Body $ Reduce (Proxy :: Proxy
                   (IslMultiAffToMap ps (Length (outerScope ++ '[k])) (Length outerScope)
                      (IdentityHeadIds ps (Length (outerScope ++ '[k])) 0 outerScope)))
                body

-- | Sum reduction over a named dimension.  Sugar for the common case
-- where the reduction monoid is 'Sum'.  Wraps the body in @PMap Sum@
-- and the result in @PMap getSum@.
sumOver
  :: forall (k :: Symbol) {outerScope :: [Symbol]} {bodyDomE :: DomE}
            {ps} {decls} {dOuter} {a}.
     ( Num a
     , KnownSymbol k
     , Elem k outerScope ~ 'False
     , KnownNat (Length outerScope)
     , KnownNat (Length (outerScope ++ '[k]))
     , KnownDom ps (Length outerScope) dOuter
     , KnownDom ps (Length (outerScope ++ '[k]))
                   ('Literal (CompileDom ps (outerScope ++ '[k]) (Length (outerScope ++ '[k])) bodyDomE))
     , KnownConstraints ps (Length (outerScope ++ '[k]) + Length outerScope)
         (IslMultiAffToMap ps (Length (outerScope ++ '[k])) (Length outerScope)
            (IdentityHeadIds ps (Length (outerScope ++ '[k])) 0 outerScope))
     , IslImageSubsetD ps (Length (outerScope ++ '[k])) (Length outerScope)
         (IslMultiAffToMap ps (Length (outerScope ++ '[k])) (Length outerScope)
            (IdentityHeadIds ps (Length (outerScope ++ '[k])) 0 outerScope))
         ('Literal (CompileDom ps (outerScope ++ '[k]) (Length (outerScope ++ '[k])) bodyDomE))
         dOuter
     )
  => DomExpr (outerScope ++ '[k]) bodyDomE
  -> Body ps decls (outerScope ++ '[k]) (Length (outerScope ++ '[k]))
          ('Literal (CompileDom ps (outerScope ++ '[k]) (Length (outerScope ++ '[k])) bodyDomE)) a
  -> Body ps decls outerScope (Length outerScope) dOuter a
sumOver _ (Body body) =
  Body $ PMap getSum $
    Reduce (Proxy :: Proxy
              (IslMultiAffToMap ps (Length (outerScope ++ '[k])) (Length outerScope)
                 (IdentityHeadIds ps (Length (outerScope ++ '[k])) 0 outerScope)))
           (PMap Sum body)


-- ═══════════════════════════════════════════════════════════════════════
-- §F. Case branches
-- ═══════════════════════════════════════════════════════════════════════

-- | Surface-level mirror of Core's 'Branches', indexed by @[DomE]@
-- instead of @[DomTag ps n]@.  'unSBranches' maps it to Core.
type SBranches
  :: forall (ps :: [Symbol]) -> [VarDecl ps]
  -> forall (scope :: [Symbol])
  -> DomTag ps (Length scope) -> [DomE] -> Type -> Type
data SBranches ps decls scope amb doms a where
  SBNil  :: SBranches ps decls scope amb '[] a
  SBCons :: forall dom ps decls scope amb doms a.
            ( KnownDom ps (Length scope)
                ('Literal (CompileDom ps scope (Length scope) dom))
            , KnownDom ps (Length scope)
                (EffectiveDomTag ('Literal (CompileDom ps scope (Length scope) dom)) amb)
            )
         => DomExpr scope dom
         -> Body ps decls scope (Length scope)
                 (EffectiveDomTag ('Literal (CompileDom ps scope (Length scope) dom)) amb) a
         -> SBranches ps decls scope amb doms a
         -> SBranches ps decls scope amb (dom ': doms) a

-- | Add a branch to a case.
when_ :: forall dom ps decls scope amb doms a.
         ( KnownDom ps (Length scope)
             ('Literal (CompileDom ps scope (Length scope) dom))
         , KnownDom ps (Length scope)
             (EffectiveDomTag ('Literal (CompileDom ps scope (Length scope) dom)) amb)
         )
      => DomExpr scope dom
      -> Body ps decls scope (Length scope)
              (EffectiveDomTag ('Literal (CompileDom ps scope (Length scope) dom)) amb) a
      -> SBranches ps decls scope amb doms a
      -> SBranches ps decls scope amb (dom ': doms) a
when_ = SBCons

-- | Build a 'Case' expression from surface branches.
caseB :: forall {ps} {decls} {scope :: [Symbol]} {amb} {doms} {a}.
         ( KnownDom ps (Length scope) amb
         , IslPartitionsD ps (Length scope) amb
             (DomsToLitList ps scope (Length scope) doms)
         )
      => SBranches ps decls scope amb doms a
      -> Body ps decls scope (Length scope) amb a
caseB bs = Body $ Case (unSBranches bs)

-- | Convert surface branches to Core branches.
unSBranches
  :: SBranches ps decls scope amb doms a
  -> Branches ps decls (Length scope) amb
       (DomsToLitList ps scope (Length scope) doms) a
unSBranches SBNil = BNil
unSBranches (SBCons (_ :: DomExpr scope dom) (Body body) rest) =
  BCons (Proxy :: Proxy ('Literal (CompileDom ps scope (Length scope) dom)))
        body
        (unSBranches rest)


-- ═══════════════════════════════════════════════════════════════════════
-- §G. Declarations, equations, and systems
-- ═══════════════════════════════════════════════════════════════════════

-- | Declare an input variable.
input :: forall (name :: Symbol) {scope :: [Symbol]} {ps} {a} {d :: DomE}.
         KnownSymbol name
      => DomExpr scope d -> Proxy a
      -> Decl ps ('VarDecl @ps @name @(Length scope)
                           @('Literal (CompileDom ps scope (Length scope) d)) @a)
input _ _ = MkDecl

-- | Declare an output variable.
output :: forall (name :: Symbol) {scope :: [Symbol]} {ps} {a} {d :: DomE}.
          KnownSymbol name
       => DomExpr scope d -> Proxy a
       -> Decl ps ('VarDecl @ps @name @(Length scope)
                            @('Literal (CompileDom ps scope (Length scope) d)) @a)
output _ _ = MkDecl

-- | Declare a local variable.
local :: forall (name :: Symbol) {scope :: [Symbol]} {ps} {a} {d :: DomE}.
         KnownSymbol name
      => DomExpr scope d -> Proxy a
      -> Decl ps ('VarDecl @ps @name @(Length scope)
                           @('Literal (CompileDom ps scope (Length scope) d)) @a)
local _ _ = MkDecl

-- | Define an equation for a declared variable.  The scope must be
-- provided via a type application: @def \@\"C\" \@'[\"i\",\"j\"] body@.
def :: forall (name :: Symbol) (scope :: [Symbol])
             {ps} {decls} {decl :: VarDecl ps}.
       ( decl ~ Lookup name decls
       , KnownSymbol name
       , Length scope ~ DeclDims decl
       )
    => Body ps decls scope (DeclDims decl) (DeclDomTag decl) (DeclType decl)
    -> Equation ps decls name
def (Body body) = Defines (Proxy @name) body

-- | Construct a system.  Thin alias for the Core 'System' pattern
-- synonym, re-exported for import convenience.
system :: forall ps inputs outputs locals defined.
          ( KnownSymbols ps
          , DefinesAllExactlyOnce (outputs ++ locals) defined
          )
       => Decls  ps inputs outputs locals
       -> EqList ps (inputs ++ outputs ++ locals) defined
       -> System ps inputs outputs locals
system = System
