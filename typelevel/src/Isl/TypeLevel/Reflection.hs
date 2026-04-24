{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Domain-tag reflection layer for the Alpha DSL.
--
-- See @doc/alpha-design.md@ §4 (Pillar 1: Mirrored ISL primitives) and
-- §5 (Pillar 2: Reflection-based domain dictionaries).  See also
-- @doc/alpha-implementation.md@ for the running deviation log.
--
-- = Design at a glance
--
-- 'DomTag' identifies a polyhedral domain at the type level.  It has
-- three constructors:
--
--   * 'Literal' carries a single type-level conjunction of constraints.
--     Used by programs the user wrote in source.
--
--   * 'LiteralU' carries a type-level disjunction of conjunctions.
--     Used by branches whose effective domain is a union.
--
--   * 'ReflectedTag' wraps a fresh skolem introduced by
--     'Data.Reflection.reify'.  Used by transforms that build a new
--     domain at runtime via ISL.
--
-- 'KnownDom' is the one-stop dictionary: it reflects a tagged domain
-- to a value-level @['Conjunction' 'SetIx']@ (the union form).  From
-- there, 'domToSet' is the sole bridge to a raw ISL 'Isl.Set' — every
-- predicate or transform that needs an ISL object for a tagged domain
-- goes through it.  'domToString' is the single text-derivation path
-- and is reserved for display (error messages, diagnostic assertions).
--
-- The three mirror predicates ('islSubsetCheck', 'islNonEmpty',
-- 'islImageSubsetCheck') take 'KnownDom' arguments, build the runtime
-- ISL sets via 'domToSet', and return either 'Bool' or a typed proof
-- token ('Maybe' ('Dict' ...)).  Reflected-tag construction is done by
-- 'reifyDomFromConjunctions', which takes a value-level disjunction
-- and hands the continuation a fresh 'ReflectedTag' phantom.
--
-- = Trust base
--
-- Two seal points:
--
--   1. 'unsafeCoerce' inside 'reifyDomFromConjunctions', lifting the
--      @s :: Type@ skolem produced by 'Data.Reflection.reify' into a
--      @'ReflectedTag' s :: DomTag ps n@ slot.  Sound by mirrored
--      construction: the 'Reifies' library guarantees @s@ is fresh
--      and nominal, and the 'KnownDom' instance for 'ReflectedTag'
--      uses exactly the dictionary @reify@ established.  The
--      same-shape 'unsafeCoerce' inside 'withPartitionsD',
--      'islSubsetCheck', and 'islImageSubsetCheck[S]' is an identity
--      cast at the value level (the target classes are method-less,
--      so their runtime dicts are unit); this does not add new trust
--      beyond the class-method-lessness invariant.
--
--   2. 'unsafePerformIO' inside 'domToString' and the retained mirror
--      predicates, used to extract pure 'String' / 'Bool' results
--      from 'IslT IO' computations.  Sound because ISL operations are
--      referentially transparent given the same ISL context.
--
-- That's all the new trust this module introduces, on top of the
-- 'Data.Reflection' library and the existing @highlevel/Set@ API.
-- v1 transforms in @alpha/@ contain *zero* additional 'unsafeCoerce'.
module Isl.TypeLevel.Reflection
  ( -- * Domain tags
    DomTag(..)
    -- * Type-level list helpers
  , Append, MapAppend
    -- * Parameter-context lifting into DomTag wrappers
  , LitPrepend, MapLitPrepend, MapPrependEach
    -- * Domain → union conversion
  , DomToUnion
    -- * Effective domain (branch ∩ ambient)
  , EffectiveDomTag
    -- * The KnownDom dictionary
  , KnownDom(..)
    -- * Domain-tag-indexed obligation classes
  , IslSubsetD
  , IslImageSubsetD
  , IslRangeOfD
  , IslCoversD
  , IslPartitionsD
  , LiteralBranches, LiteralBranchesU
    -- * D-from-U evidence combinators
  , withPartitionsD
    -- * Reflected route
  , reifyDomFromConjunctions
    -- * Proof tokens (v6 — see deviation D3)
  , Dict(..)
    -- * ISL object construction from a KnownDom dictionary
  , domToSet
  , domToString
    -- * Mirror predicates
  , islSubsetCheck
  , islImageSubsetCheck
  , islImageSubsetCheckS
  , islImageSubsetCheckSPctx
  , islNonEmpty
  , checkParamCtx
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict
import Data.Proxy (Proxy(..))
import qualified Data.Reflection as R
import GHC.TypeLits
  ( KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal, type (+) )
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Typed.Constraints (SetIx, Conjunction(..))
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols, Length, symbolVals)
import qualified Isl.Set as RawS
import qualified Isl.Map as RawM
import qualified Isl.Space as Space
import qualified Isl.Types as Isl
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_)
import qualified Isl.Linear as Isl
import Isl.TypeLevel.Constraint
  ( IslCoversU
  , IslImageSubset
  , IslImageSubsetU
  , IslMapToString
  , IslPartitionsU
  , IslRangeOf
  , IslSubset
  , IslSubsetU
  , LiftPctxN
  , TConstraint
  )
import Isl.TypeLevel.Sing
  ( KnownConstraints, knownConstraints, reifySTConstraintsSet
  , KnownDisjunction, knownDisjunction, reifySTDisjunctionSet
  )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. The DomTag kind
-- ═══════════════════════════════════════════════════════════════════════

-- | A domain tag identifies a polyhedron at the type level.  See the
-- module preamble for the design overview.
type DomTag :: forall (ps :: [Symbol]) -> forall (n :: Nat) -> Type
data DomTag ps n where
  Literal      :: forall ps n. [TConstraint ps n] -> DomTag ps n
  LiteralU     :: forall ps n. [[TConstraint ps n]] -> DomTag ps n
  ReflectedTag :: forall ps n. Type -> DomTag ps n


-- | Type-level list concatenation (local to avoid pulling in
-- @Data.Type.List@ or @singletons@ for one function).
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Append @extra@ to each element of a list of lists.
-- Used to distribute an ambient conjunction over each disjunct:
-- @(A₁ ∪ A₂) ∩ B = (A₁ ∩ B) ∪ (A₂ ∩ B)@.
type family MapAppend (css :: [[k]]) (extra :: [k]) :: [[k]] where
  MapAppend '[] _ = '[]
  MapAppend (cs ': css) extra = Append cs extra ': MapAppend css extra

-- | Prepend a conjunction @cs@ to every branch of a disjunction.
-- Dual to 'MapAppend' at the head: @cs ∩ (B₁ ∪ B₂) = (cs ∩ B₁) ∪ (cs ∩ B₂)@.
type family MapPrependEach (cs :: [k]) (css :: [[k]]) :: [[k]] where
  MapPrependEach _  '[]         = '[]
  MapPrependEach cs (bs ': rest) = Append cs bs ': MapPrependEach cs rest

-- | Fuse a parameter-derived conjunction @cs@ into a Literal-shaped
-- DomTag wrapper.  For 'Literal', prepends @cs@ to the single
-- conjunction.  For 'LiteralU', prepends @cs@ to each disjunct.
-- Reflected tags are untouched (the runtime reified set is not
-- statically indexed by a constraint list).
type family LitPrepend (cs :: [TConstraint ps n]) (d :: DomTag ps n)
                    :: DomTag ps n where
  LitPrepend cs ('Literal bs)    = 'Literal (Append cs bs)
  LitPrepend cs ('LiteralU css)  = 'LiteralU (MapPrependEach cs css)
  LitPrepend _  ('ReflectedTag s) = 'ReflectedTag s

-- | Apply 'LitPrepend' with the same conjunction @cs@ to every
-- element of a list of DomTags.  Used by 'Case' to fuse the lifted
-- pctx into the ambient and each branch domain.
type family MapLitPrepend (cs :: [TConstraint ps n]) (ds :: [DomTag ps n])
                       :: [DomTag ps n] where
  MapLitPrepend _  '[]       = '[]
  MapLitPrepend cs (d ': ds) = LitPrepend cs d ': MapLitPrepend cs ds

-- | Convert any 'DomTag' to its union representation.
type family DomToUnion (d :: DomTag ps n) :: [[TConstraint ps n]] where
  DomToUnion ('Literal cs) = '[cs]
  DomToUnion ('LiteralU css) = css

-- | A runtime-constructible proof token for a constraint.  Used by the
-- v6 reflected-route mirror predicates as the return type: a
-- @'Just' 'Dict'@ brings the obligation into scope as a local given,
-- which is the only way a transform body can construct an AST node
-- whose plugin-dispatched obligation class has no instance for the
-- reflected tag the transform just produced.  See deviation D3 in
-- @doc/alpha-implementation.md@ for the soundness story and the
-- interaction with the class-method-lessness invariant.
--
-- Inlined here instead of depending on @constraints@ to keep the
-- trust base contained in a single module.
type Dict :: Constraint -> Type
data Dict c where
  Dict :: c => Dict c

-- | The effective domain of a @Case@ branch: the declared branch tag
-- @d@ intersected with the ambient @amb@ (the recurrence domain).
--
-- Used by 'Alpha.Core.BCons' to type the branch body.  Reads inside
-- the body run their 'IslImageSubset' obligations against @d ∩ amb@
-- rather than the raw declared @d@, which captures the fact that a
-- branch guard is implicitly intersected with the ambient at
-- evaluation time — an "over-wide" declared branch is semantically
-- identical to its clip.
--
-- For the literal/literal case (all v5 uses 'Literal' tags) the
-- intersection is simple conjunction concatenation of the two
-- constraint lists: @'Literal' (dCs ++ aCs)@.  A reflected-tag
-- intersection would require a new value-level primitive producing a
-- fresh reflected tag from two existing tags — deferred until a
-- consumer actually needs it (see deviation D3).
type family EffectiveDomTag (d :: DomTag ps n) (amb :: DomTag ps n)
         :: DomTag ps n where
  EffectiveDomTag ('Literal dCs) ('Literal aCs) = 'Literal (Append dCs aCs)
  EffectiveDomTag ('LiteralU dCss) ('Literal aCs) = 'LiteralU (MapAppend dCss aCs)


-- ═══════════════════════════════════════════════════════════════════════
-- §2. The KnownDom class
-- ═══════════════════════════════════════════════════════════════════════

-- | A 'KnownDom' dictionary reflects a tagged domain to its value-level
-- disjunction of conjunctions.  All three tag constructors converge on
-- one API, which is then consumed by 'domToSet' to materialise a raw
-- ISL object.
class KnownDom (ps :: [Symbol]) (n :: Nat) (d :: DomTag ps n) where
  -- | Single-conjunction constraint list.  Errors for union/reflected
  -- tags unless the reflected disjunction happens to be a singleton.
  reflectDomConstraints :: [C.Constraint SetIx]

  -- | The domain as a list of conjunctions (union-safe, total).
  reflectDomConjunctions :: [Conjunction SetIx]

-- | Literal route: a single conjunction reflected via 'KnownConstraints'.
instance KnownConstraints ps n cs => KnownDom ps n ('Literal cs) where
  reflectDomConstraints = reifySTConstraintsSet (knownConstraints @ps @n @cs)
  reflectDomConjunctions =
    [Conjunction (reifySTConstraintsSet (knownConstraints @ps @n @cs))]

-- | Union literal route: a disjunction reflected via 'KnownDisjunction'.
instance KnownDisjunction ps n css => KnownDom ps n ('LiteralU css) where
  reflectDomConstraints =
    error "reflectDomConstraints: not available for union domains (use reflectDomConjunctions)"
  reflectDomConjunctions = reifySTDisjunctionSet (knownDisjunction @ps @n @css)

-- | Reflected route: the payload is the already-computed disjunction.
-- 'reflectDomConstraints' succeeds only when the disjunction is a
-- singleton, matching the same partiality contract as 'LiteralU'.
instance R.Reifies s [Conjunction SetIx] => KnownDom ps n ('ReflectedTag s) where
  reflectDomConstraints = case R.reflect (Proxy @s) of
    [Conjunction cs] -> cs
    _ ->
      error "reflectDomConstraints: not available for reflected union domains (use reflectDomConjunctions)"
  reflectDomConjunctions = R.reflect (Proxy @s)


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Domain-tag-indexed obligation classes
-- ═══════════════════════════════════════════════════════════════════════

class IslSubsetD (ps :: [Symbol]) (n :: Nat)
                 (d1 :: DomTag ps n) (d2 :: DomTag ps n)

instance IslSubset ps n cs1 cs2
  => IslSubsetD ps n ('Literal cs1) ('Literal cs2)

instance IslSubsetU ps n css1 '[cs2]
  => IslSubsetD ps n ('LiteralU css1) ('Literal cs2)

instance IslSubsetU ps n '[cs1] css2
  => IslSubsetD ps n ('Literal cs1) ('LiteralU css2)

instance IslSubsetU ps n css1 css2
  => IslSubsetD ps n ('LiteralU css1) ('LiteralU css2)


-- | Domain-tag-indexed partition dispatch.
class IslPartitionsD (ps :: [Symbol]) (n :: Nat)
                     (d :: DomTag ps n)
                     (branches :: [DomTag ps n])

instance IslPartitionsU ps n '[cs] (LiteralBranchesU branches)
  => IslPartitionsD ps n ('Literal cs) branches

instance IslPartitionsU ps n css (LiteralBranchesU branches)
  => IslPartitionsD ps n ('LiteralU css) branches


class IslImageSubsetD (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                      (mapCs :: [TConstraint ps (ni + no)])
                      (dSrc :: DomTag ps ni) (dDst :: DomTag ps no)

instance IslImageSubset ps ni no mapCs cs1 cs2
  => IslImageSubsetD ps ni no mapCs ('Literal cs1) ('Literal cs2)

instance IslImageSubsetU ps ni no mapCs css dstCs
  => IslImageSubsetD ps ni no mapCs ('LiteralU css) ('Literal dstCs)


class IslRangeOfD (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                  (mapCs :: [TConstraint ps (ni + no)])
                  (d :: DomTag ps no)

instance IslRangeOf ps ni no mapCs cs
  => IslRangeOfD ps ni no mapCs ('Literal cs)


class IslCoversD (ps :: [Symbol]) (n :: Nat)
                 (d :: DomTag ps n)
                 (branches :: [DomTag ps n])

instance IslCoversU ps n '[cs] (LiteralBranchesU branches)
  => IslCoversD ps n ('Literal cs) branches

instance IslCoversU ps n css (LiteralBranchesU branches)
  => IslCoversD ps n ('LiteralU css) branches


-- | Construct 'IslPartitionsD' evidence from 'IslPartitionsU'.
--
-- Sound: 'IslPartitionsD' is method-less; its instances require exactly
-- 'IslPartitionsU' as premise; the runtime dictionary is a pointer to
-- the superclass evidence already in scope.
--
-- Needed because GHC does not reliably present superclass wanteds from
-- instance heads to the plugin solver (see caseWithElsewhere path).
withPartitionsD
  :: forall ps n d branchDoms r.
     IslPartitionsU ps n (DomToUnion d) (LiteralBranchesU branchDoms)
  => (IslPartitionsD ps n d branchDoms => r)
  -> r
withPartitionsD f =
  case unsafeCoerce (Dict @()) :: Dict (IslPartitionsD ps n d branchDoms) of
    Dict -> f
{-# INLINE withPartitionsD #-}


-- | Extract branch constraint lists (flat, for legacy use).
type family LiteralBranches (ds :: [DomTag ps n]) :: [[TConstraint ps n]] where
  LiteralBranches '[] = '[]
  LiteralBranches ('Literal cs ': rest) = cs ': LiteralBranches rest

-- | Extract per-branch disjunctions (union-aware).
type family LiteralBranchesU (ds :: [DomTag ps n]) :: [[[TConstraint ps n]]] where
  LiteralBranchesU '[] = '[]
  LiteralBranchesU ('Literal cs ': rest) = '[cs] ': LiteralBranchesU rest
  LiteralBranchesU ('LiteralU css ': rest) = css ': LiteralBranchesU rest


-- ═══════════════════════════════════════════════════════════════════════
-- §4. The reifyDom helper
-- ═══════════════════════════════════════════════════════════════════════

-- | Smuggle a runtime disjunction of conjunctions into a fresh phantom
-- 'DomTag' tag and run a continuation that has access to the resulting
-- 'KnownDom' dictionary.
--
-- The trusted seal lives entirely inside 'Data.Reflection.reify'
-- (which uses 'unsafeCoerce' once to introduce a fresh skolem).
-- Our additional 'unsafeCoerce' lifts the library's @s :: Type@ skolem
-- into the @'ReflectedTag s :: DomTag ps n@ slot — this is the only
-- 'unsafeCoerce' in this module's reflected-route code path.
reifyDomFromConjunctions
  :: forall ps n r.
     ( KnownNat n
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => [Conjunction SetIx]
  -> ( forall (d :: DomTag ps n). KnownDom ps n d => Proxy d -> r )
  -> r
reifyDomFromConjunctions cs k =
  R.reify cs $ \(_ :: Proxy s) ->
    k (unsafeCoerce (Proxy @('ReflectedTag s))
        :: Proxy ('ReflectedTag s :: DomTag ps n))


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Boundary helpers: KnownDom dict → raw ISL object / text
-- ═══════════════════════════════════════════════════════════════════════

-- | Build an ISL 'Isl.Set' from a 'KnownDom' dictionary.  The sole
-- bridge from a typed 'DomTag' to a raw ISL object: every predicate or
-- transform that needs an ISL 'Isl.Set' for a tagged domain goes
-- through this.
--
-- The empty-disjunction case builds an empty 'Isl.Set' directly from
-- a parameter-named 'Isl.Space'.  The non-empty case folds
-- 'C.buildBasicSet' + 'RawS.fromBasicSet' + 'RawS.union' over the
-- disjunction, matching the plugin's 'buildUnionSetFromDisj' shape.
domToSet
  :: forall ps n d m.
     ( KnownDom ps n d
     , KnownSymbols ps
     , KnownNat n
     , MonadIO m
     )
  => IslT m Isl.Set
domToSet = Isl.do
  let conjs    = reflectDomConjunctions @ps @n @d
      paramNs  = symbolVals @ps
      nParams  = length paramNs
      nVal     = fromIntegral (natVal (Proxy @n)) :: Int
  case conjs of
    [] -> Isl.do
      space0 <- Space.setAlloc (fromIntegral nParams) (fromIntegral nVal)
      space  <- Isl.foldM
                  (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                  space0 (zip [0..] paramNs)
      RawS.empty space
    (c0 : cs) -> Isl.do
      bs0 <- C.buildBasicSet paramNs nVal c0
      s0  <- RawS.fromBasicSet bs0
      Isl.foldM
        (\s c' -> Isl.do
            bs <- C.buildBasicSet paramNs nVal c'
            s' <- RawS.fromBasicSet bs
            RawS.union s s')
        s0 cs

-- | Derived ISL-text form of a tagged domain.  For display only
-- (error messages, diagnostic assertions).  Replaces the old
-- @reflectDomString@ method on 'KnownDom'.
domToString
  :: forall ps n d.
     ( KnownDom ps n d
     , KnownSymbols ps
     , KnownNat n
     )
  => String
domToString = unsafePerformIO $ runIslT $ Isl.do
  s <- domToSet @ps @n @d
  Ur str <- query_ s RawS.toStr
  Isl.pure (Ur str)


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Mirror predicates — the pillar-1 catalog
-- ═══════════════════════════════════════════════════════════════════════
--
-- Each predicate builds its sets via 'domToSet' (inside a single
-- 'runIslT') and extracts a pure 'Bool' through 'unsafePerformIO'.
-- Soundness: ISL predicates are pure given the same context, and the
-- linear discipline in @highlevel/Set@ is resource-tracking, not
-- semantic; treating it as unrestricted in this contained scope is
-- safe.

-- | Predicate mirror: check whether @d1@ is a subset of @d2@ at runtime,
-- returning a typed proof token on success.  The value-level mirror of
-- the plugin's 'IslSubset' wanted-class handler.
--
-- v6 changed the return type from @'Bool'@ to @'Maybe' ('Dict' ('IslSubsetD'
-- ps n d1 d2))@: a successful check now hands the caller the exact
-- obligation it proved, which the caller can pattern-match via @'Just'
-- 'Dict'@ to bring the constraint into scope as a local given.  This is
-- how tile2D in @alpha/@ constructs AST nodes whose plugin-dispatched
-- obligations fire on a reflected tag without needing globally-visible
-- "trust-me" instances.  See deviation D3 in
-- @doc/alpha-implementation.md@ for the full design.
--
-- Soundness note: the @'unsafeCoerce'@ inside 'subsetDict' is
-- operationally a no-op because 'IslSubsetD' is method-less and GHC
-- represents its runtime dictionary as the unit value.  Relabeling
-- @'Dict' '()'@ as @'Dict' ('IslSubsetD' ...)@ is therefore an identity
-- cast at the value level.  If a future change adds a nullary method
-- to 'IslSubsetD' (mirroring the D15 fix on the underlying
-- plugin-checked classes in @"Isl.TypeLevel.Constraint"@), the runtime
-- dict gains a field and the unit-coerce stops being sound — the fix
-- is to coerce from a concretely-solvable literal-route instantiation
-- of 'IslSubsetD' instead, producing a real dict with the right shape
-- that 'unsafeCoerce' only relabels.  Document any such upgrade in
-- sync across all @...Dict@ helpers in this module.
islSubsetCheck
  :: forall ps n d1 d2.
     ( KnownDom ps n d1
     , KnownDom ps n d2
     , KnownNat n
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => Proxy d1
  -> Proxy d2
  -> Maybe (Dict (IslSubsetD ps n d1 d2))
islSubsetCheck _ _ =
  let ok = unsafePerformIO $ runIslT $ Isl.do
             s1 <- domToSet @ps @n @d1
             Ur b <- Isl.queryM_ s1 (\r1 -> Isl.do
               s2 <- domToSet @ps @n @d2
               Ur r <- query_ s2 (\r2 -> RawS.isSubset r1 r2)
               Isl.pure (Ur r))
             Isl.pure (Ur b)
   in if ok then Just subsetDict else Nothing
  where
    subsetDict :: Dict (IslSubsetD ps n d1 d2)
    subsetDict = unsafeCoerce (Dict :: Dict (() :: Constraint))

-- | Predicate mirror for 'IslImageSubsetD': check that the image of
-- @dSrc@ under the map given as an ISL string is a subset of @dDst@.
-- Returns a typed proof token on success.
--
-- This is what tile2D calls once per rewritten 'Alpha.Core.Dep' node
-- before constructing it.  Each @'Just' 'Dict'@ is a local precondition
-- that makes the subsequent 'Alpha.Core.Dep' construction typecheck
-- against a reflected source tag; without the 'Dict', instance
-- resolution for 'IslImageSubsetD' on a reflected source fails.
--
-- See 'islSubsetCheck' for the soundness discussion of the internal
-- @'unsafeCoerce'@ helper; the same method-lessness invariant applies.
islImageSubsetCheck
  :: forall ps ni no (mapCs :: [TConstraint ps (ni + no)]) dSrc dDst.
     ( KnownDom ps ni dSrc
     , KnownDom ps no dDst
     , KnownNat ni
     , KnownNat no
     , KnownSymbols ps
     , KnownNat (Length ps)
     , KnownSymbol (IslMapToString ps ni no mapCs)
     )
  => Proxy dSrc
  -> Proxy dDst
  -> Maybe (Dict (IslImageSubsetD ps ni no mapCs dSrc dDst))
islImageSubsetCheck _ _ =
  let mapStr = symbolVal (Proxy @(IslMapToString ps ni no mapCs))
      ok = unsafePerformIO $ runIslT $ Isl.do
             src <- domToSet @ps @ni @dSrc
             m   <- RawM.readFromStr mapStr
             img <- RawS.apply src m
             Ur b <- Isl.queryM_ img (\imgRef -> Isl.do
               dst <- domToSet @ps @no @dDst
               Ur r <- query_ dst (\dstRef -> RawS.isSubset imgRef dstRef)
               Isl.pure (Ur r))
             Isl.pure (Ur b)
   in if ok then Just imgDict else Nothing
  where
    imgDict :: Dict (IslImageSubsetD ps ni no mapCs dSrc dDst)
    imgDict = unsafeCoerce (Dict :: Dict (() :: Constraint))

-- | String-supplied variant of 'islImageSubsetCheck'.
--
-- Identical in function but takes the map's ISL text as an explicit
-- @'String'@ argument instead of computing it via
-- @KnownSymbol (IslMapToString ps ni no mapCs)@.  Used when @mapCs@
-- mentions skolem type variables (e.g., tile factors lifted via
-- @'GHC.TypeLits.someNatVal'@) that the plugin cannot reduce to a
-- concrete 'Symbol' at compile time, but whose value-level equivalent
-- the caller already has in scope.
--
-- The caller is responsible for ensuring the supplied @mapStr@
-- matches what @'IslMapToString' ps ni no mapCs@ *would* produce if
-- the plugin were able to reduce it — i.e., both representations
-- must denote the same ISL map.  This is an extra soundness
-- obligation beyond what the plugin-computed variant carries; the
-- v6.1 use in "Alpha.Transform.Loop.tile2DCross" discharges it by
-- constructing both the type-level @TilingInverseMap tiN tjN@ phantom
-- and the value-level map string from the same runtime @ti@/@tj@
-- integers (after 'someNatVal' pins them), so they describe the same
-- parametric map by construction.
--
-- The returned 'Dict' has the same type index as 'islImageSubsetCheck',
-- so downstream callers that pattern-match @'Just' 'Dict'@ can
-- construct the corresponding AST node (typically an
-- @'Alpha.Core.Dep'@) and rely on the type-level @mapCs@ phantom.
-- The internal @unsafeDict@ helper (same form as
-- 'islImageSubsetCheck') produces the evidence.
islImageSubsetCheckS
  :: forall ps ni no (mapCs :: [TConstraint ps (ni + no)]) dSrc dDst.
     ( KnownDom ps ni dSrc
     , KnownDom ps no dDst
     , KnownNat ni
     , KnownNat no
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => String            -- ^ map ISL text (caller-supplied; see note above)
  -> Proxy dSrc
  -> Proxy dDst
  -> Maybe (Dict (IslImageSubsetD ps ni no mapCs dSrc dDst))
islImageSubsetCheckS mapStr _ _ =
  let ok = unsafePerformIO $ runIslT $ Isl.do
             src <- domToSet @ps @ni @dSrc
             m   <- RawM.readFromStr mapStr
             img <- RawS.apply src m
             Ur b <- Isl.queryM_ img (\imgRef -> Isl.do
               dst <- domToSet @ps @no @dDst
               Ur r <- query_ dst (\dstRef -> RawS.isSubset imgRef dstRef)
               Isl.pure (Ur r))
             Isl.pure (Ur b)
   in if ok then Just imgDict else Nothing
  where
    imgDict :: Dict (IslImageSubsetD ps ni no mapCs dSrc dDst)
    imgDict = unsafeCoerce (Dict :: Dict (() :: Constraint))

-- | Like 'islImageSubsetCheckS' but returns a 'Dict' for the
-- /pctx-fused/ obligation shape.  Used by transforms that reconstruct
-- 'Alpha.Core.Dep' nodes, whose plugin obligation is on
-- @(LiftPctxN (ni+no) pctx ++ mapCs)@ /
-- @(LitPrepend (LiftPctxN ni pctx) dSrc)@ /
-- @(LitPrepend (LiftPctxN no pctx) dDst)@.
--
-- Runs the ISL check on the /bare/ map and /bare/ DomTags: the
-- parameter-only constraints in @pctx@ (after lifting) are vacuous
-- w.r.t. the set/map dim algebra, so the ISL result is identical to
-- the fused form at every valid parameter assignment.  The returned
-- 'Dict' is shaped for the fused types, bypassing 'KnownDom' resolution
-- on the fused wrappers.
--
-- Sound by the same unsafe-seal argument as 'islImageSubsetCheckS':
-- the obligation classes are method-less, their dictionaries are unit
-- at runtime, and the ISL-level equivalence between fused and unfused
-- image-subset is parametric.
islImageSubsetCheckSPctx
  :: forall ps pctx ni no
            (mapCs :: [TConstraint ps (ni + no)])
            dSrc dDst.
     ( KnownDom ps ni dSrc
     , KnownDom ps no dDst
     , KnownNat ni
     , KnownNat no
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => String            -- ^ map ISL text
  -> Proxy pctx
  -> Proxy dSrc
  -> Proxy dDst
  -> Maybe (Dict (IslImageSubsetD ps ni no
                    (Append (LiftPctxN (ni + no) pctx) mapCs)
                    (LitPrepend (LiftPctxN ni pctx) dSrc)
                    (LitPrepend (LiftPctxN no pctx) dDst)))
islImageSubsetCheckSPctx mapStr _ _ _ =
  let ok = unsafePerformIO $ runIslT $ Isl.do
             src <- domToSet @ps @ni @dSrc
             m   <- RawM.readFromStr mapStr
             img <- RawS.apply src m
             Ur b <- Isl.queryM_ img (\imgRef -> Isl.do
               dst <- domToSet @ps @no @dDst
               Ur r <- query_ dst (\dstRef -> RawS.isSubset imgRef dstRef)
               Isl.pure (Ur r))
             Isl.pure (Ur b)
   in if ok then Just imgDict else Nothing
  where
    imgDict :: Dict (IslImageSubsetD ps ni no
                       (Append (LiftPctxN (ni + no) pctx) mapCs)
                       (LitPrepend (LiftPctxN ni pctx) dSrc)
                       (LitPrepend (LiftPctxN no pctx) dDst))
    imgDict = unsafeCoerce (Dict :: Dict (() :: Constraint))

-- | Predicate mirror: is the set labelled by @d@ non-empty?  Used by
-- the v6 'tile2D' transform to detect "tile factor too large" (the
-- tiled domain has no integer points, so the recurrence never fires).
--
-- Returns a plain @'Bool'@ rather than a 'Maybe' 'Dict' because
-- non-emptiness is not itself a dispatch-class obligation in
-- "Alpha.Surface.Core"; the transform translates a 'False' result into
-- @'EmptyResultDomain'@ on the 'Left' path.
islNonEmpty
  :: forall ps n d.
     ( KnownDom ps n d
     , KnownNat n
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => Proxy d
  -> Bool
islNonEmpty _ =
  let isEmpty = unsafePerformIO $ runIslT $ Isl.do
                  s <- domToSet @ps @n @d
                  Ur b <- query_ s RawS.isEmpty
                  Isl.pure (Ur b)
   in not isEmpty


-- | Runtime check that the supplied parameter assignment satisfies the
-- type-level parameter context @pctx@.  Used by @Alpha.Interpret@ —
-- @IslNonEmpty ps 0 pctx@ guarantees the pctx is satisfiable by /some/
-- assignment, but not by the concrete 'Map String Int' passed at
-- interpret time.
checkParamCtx
  :: forall ps pctx.
     ( KnownSymbols ps
     , KnownConstraints ps 0 pctx
     )
  => Data.Map.Strict.Map String Int
  -> Either String ()
checkParamCtx params =
  let paramNames = symbolVals @ps
      pctxCs     = reifySTConstraintsSet (knownConstraints @ps @0 @pctx)
      -- Equality constraints @P_i = v_i@ for every param in the map.
      paramEqs =
        [ C.EqualityConstraint
            (C.Add (C.Ix (C.SetParam i)) (C.Constant (fromIntegral (negate v))))
        | (i, n) <- zip [0..] paramNames
        , Just v <- [Data.Map.Strict.lookup n params]
        ]
      conj   = Conjunction (pctxCs ++ paramEqs)
      empty' = unsafePerformIO $ runIslT $ Isl.do
        bs <- C.buildBasicSet paramNames 0 conj
        s0 <- RawS.fromBasicSet bs
        Ur b <- query_ s0 RawS.isEmpty
        Isl.pure (Ur b)
  in if empty'
       then Left ("params = " ++ show (Data.Map.Strict.toAscList params))
       else Right ()
