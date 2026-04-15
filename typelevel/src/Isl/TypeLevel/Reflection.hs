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
-- two constructors:
--
--   * 'Literal' carries a type-level constraint list.  Used by
--     programs the user wrote in source.
--
--   * 'ReflectedTag' wraps a fresh skolem introduced by
--     'Data.Reflection.reify'.  Used by transforms that build a new
--     domain at runtime via ISL.
--
-- 'KnownDom' is a one-method class whose method 'reflectDomString'
-- returns the ISL string representation of the tagged domain.  This
-- representation is unrestricted (a plain Haskell 'String'), avoiding
-- the linear-types plumbing of @highlevel/Set@ entirely.  The two
-- routes converge: literal tags use 'IslToString' (a plugin-rewritten
-- type family that computes the string at GHC compile time); reflected
-- tags read the string out of a 'Reifies' dictionary.  Consumers see
-- one API.
--
-- The mirror functions ('islUnion', 'islIntersect', 'islSubsetCheck')
-- take 'KnownDom' arguments, build the runtime ISL sets via
-- 'fromString', perform the operation, and return either a fresh
-- reflected tag (CPS-style) or a 'Bool'.  The CPS form is what makes
-- the 'reifyDom' skolem stay scoped to the continuation.
--
-- = Trust base
--
-- Two seal points:
--
--   1. 'unsafeCoerce' inside 'reifyDom', lifting the @s :: Type@
--      skolem produced by 'Data.Reflection.reify' into a
--      @'ReflectedTag' s :: DomTag ps n@ slot.  Sound by mirrored
--      construction: the 'Reifies' library guarantees @s@ is fresh
--      and nominal, and the 'KnownDom' instance for 'ReflectedTag'
--      uses exactly the dictionary @reify@ established.
--
--   2. 'unsafePerformIO' inside the mirror functions to extract pure
--      'String' / 'Bool' results from 'IslT IO' computations.  Sound
--      because ISL operations are referentially transparent given
--      the same ISL context.
--
-- That's all the new trust this module introduces, on top of the
-- 'Data.Reflection' library and the existing @highlevel/Set@ API.
-- v1 transforms in @alpha/@ contain *zero* additional 'unsafeCoerce'.
module Isl.TypeLevel.Reflection
  ( -- * Domain tags
    DomTag(..)
    -- * Type-level list helpers
  , Append
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
  , LiteralBranches
    -- * Reflected route
  , reifyDomFromString
    -- * Proof tokens (v6 — see deviation D3)
  , Dict(..)
    -- * Mirror functions
  , islUnion
  , islIntersect
  , islApply
  , islSubsetCheck
  , islImageSubsetCheck
  , islImageSubsetCheckS
  , islNonEmpty
  ) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import qualified Data.Reflection as R
import GHC.TypeLits
  ( KnownNat, KnownSymbol, Nat, Symbol, symbolVal, type (+) )
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Typed.Constraints (SetIx)
import qualified Isl.Typed.Constraints as C
import Isl.Typed.Params (KnownSymbols, Length)
import qualified Isl.Set as RawS
import qualified Isl.Map as RawM
import Isl.Monad (IslT, Ur(..), runIslT)
import Isl.Linear (query_, freeM)
import qualified Isl.Linear as Isl
import qualified Isl.Types as Isl
import Isl.TypeLevel.Constraint
  ( IslCovers
  , IslImageSubset
  , IslMapToString
  , IslPartitions
  , IslRangeOf
  , IslSubset
  , IslToString
  , TConstraint
  )
import Isl.TypeLevel.Sing
  ( KnownConstraints, knownConstraints, reifySTConstraintsSet )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. The DomTag kind
-- ═══════════════════════════════════════════════════════════════════════

-- | A domain tag identifies a polyhedron at the type level.  See the
-- module preamble for the design overview.
type DomTag :: forall (ps :: [Symbol]) -> forall (n :: Nat) -> Type
data DomTag ps n where
  Literal      :: forall ps n. [TConstraint ps n] -> DomTag ps n
  ReflectedTag :: forall ps n. Type -> DomTag ps n


-- | Type-level list concatenation (local to avoid pulling in
-- @Data.Type.List@ or @singletons@ for one function).
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

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


-- ═══════════════════════════════════════════════════════════════════════
-- §2. The KnownDom class
-- ═══════════════════════════════════════════════════════════════════════

-- | A 'KnownDom' dictionary returns the ISL string representation of
-- the tagged domain.  The string is unrestricted ('String' is a plain
-- Haskell value), avoiding the linear-types story of @highlevel/Set@
-- entirely.
class KnownDom (ps :: [Symbol]) (n :: Nat) (d :: DomTag ps n) where
  -- | The ISL textual form of the tagged domain.  For literal tags
  -- this is computed at GHC compile time by the plugin's
  -- 'IslToString' rewriter; for reflected tags it is read from a
  -- 'Reifies' dictionary that 'reifyDomFromString' established.
  reflectDomString :: String

  -- | The domain's constraint list as value-level 'Constraint's.
  -- For literal tags, these are reflected directly from the type-level
  -- constraint list via 'KnownConstraints'.  For reflected tags, this
  -- is not available (errors at runtime).
  reflectDomConstraints :: [C.Constraint SetIx]

-- | Literal route: the plugin computes the ISL string at compile
-- time via the 'IslToString' type family, and 'KnownSymbol' gives us
-- the runtime String.  The constraint list is reflected via
-- 'KnownConstraints'.
instance
  ( KnownSymbol (IslToString ps n cs)
  , KnownConstraints ps n cs
  ) => KnownDom ps n ('Literal cs) where
  reflectDomString = symbolVal (Proxy @(IslToString ps n cs))
  reflectDomConstraints = reifySTConstraintsSet (knownConstraints @ps @n @cs)

-- | Reflected route: 'reifyDomFromString' established a 'Reifies'
-- dictionary keyed by the fresh skolem @s@; we read the string out
-- of it.  Constraint reflection is not available for reflected tags.
instance R.Reifies s String => KnownDom ps n ('ReflectedTag s) where
  reflectDomString = R.reflect (Proxy @s)
  reflectDomConstraints =
    error "reflectDomConstraints: not available for reflected domains"


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Domain-tag-indexed obligation classes
-- ═══════════════════════════════════════════════════════════════════════

class IslSubsetD (ps :: [Symbol]) (n :: Nat)
                 (d1 :: DomTag ps n) (d2 :: DomTag ps n)

instance IslSubset ps n cs1 cs2
  => IslSubsetD ps n ('Literal cs1) ('Literal cs2)


-- | Domain-tag-indexed 'IslPartitions' dispatch.  Checks that the
-- branch domains of a 'Alpha.Core.Case' both cover the ambient @d@
-- and are pairwise disjoint within it — every point in the ambient
-- is defined by exactly one branch.  See v5.2 in
-- @doc/alpha-implementation.md@.
class IslPartitionsD (ps :: [Symbol]) (n :: Nat)
                     (d :: DomTag ps n)
                     (branches :: [DomTag ps n])

instance ( IslPartitions ps n '[cs] (LiteralBranches branches) )
  => IslPartitionsD ps n ('Literal cs) branches


class IslImageSubsetD (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                      (mapCs :: [TConstraint ps (ni + no)])
                      (dSrc :: DomTag ps ni) (dDst :: DomTag ps no)

instance IslImageSubset ps ni no mapCs cs1 cs2
  => IslImageSubsetD ps ni no mapCs ('Literal cs1) ('Literal cs2)


class IslRangeOfD (ps :: [Symbol]) (ni :: Nat) (no :: Nat)
                  (mapCs :: [TConstraint ps (ni + no)])
                  (d :: DomTag ps no)

instance IslRangeOf ps ni no mapCs cs
  => IslRangeOfD ps ni no mapCs ('Literal cs)


class IslCoversD (ps :: [Symbol]) (n :: Nat)
                 (d :: DomTag ps n)
                 (branches :: [DomTag ps n])

instance ( IslCovers ps n '[cs] (LiteralBranches branches) )
  => IslCoversD ps n ('Literal cs) branches


type family LiteralBranches (ds :: [DomTag ps n]) :: [[TConstraint ps n]] where
  LiteralBranches '[] = '[]
  LiteralBranches ('Literal cs ': rest) = cs ': LiteralBranches rest


-- ═══════════════════════════════════════════════════════════════════════
-- §4. The reifyDom helper
-- ═══════════════════════════════════════════════════════════════════════

-- | Smuggle a runtime ISL string into a fresh phantom 'DomTag' tag and
-- run a continuation that has access to the resulting 'KnownDom'
-- dictionary.
--
-- The trusted seal lives entirely inside 'Data.Reflection.reify'
-- (which uses 'unsafeCoerce' once to introduce a fresh skolem).
-- Our additional 'unsafeCoerce' lifts the library's @s :: Type@ skolem
-- into the @'ReflectedTag s :: DomTag ps n@ slot — this is the only
-- 'unsafeCoerce' in this module's reflected-route code path.
reifyDomFromString
  :: forall ps n r.
     ( KnownNat n
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => String
  -> ( forall (d :: DomTag ps n). KnownDom ps n d => Proxy d -> r )
  -> r
reifyDomFromString str k =
  R.reify str $ \(_ :: Proxy s) ->
    k (unsafeCoerce (Proxy @('ReflectedTag s))
        :: Proxy ('ReflectedTag s :: DomTag ps n))


-- ═══════════════════════════════════════════════════════════════════════
-- §5. Internal helpers for unrestricted ISL string operations
-- ═══════════════════════════════════════════════════════════════════════
--
-- These wrap the linear @highlevel/Set@ API in 'unsafePerformIO' so we
-- can produce pure 'String' / 'Bool' results.  Soundness: ISL
-- operations are pure given the same context, and the linear types in
-- @highlevel/Set@ exist for resource discipline (avoiding double-free
-- of the underlying ISL handles), not for semantic invariants — so
-- treating them as unrestricted in this contained scope is safe.

-- | Parse two ISL set strings, apply a binary set operation, and
-- return the result string.
binopString
  :: (Isl.Set %1 -> Isl.Set %1 -> IslT IO Isl.Set)
  -> String -> String -> String
binopString op str1 str2 = unsafePerformIO $ runIslT $ Isl.do
  s1 <- RawS.readFromStr str1
  s2 <- RawS.readFromStr str2
  s3 <- op s1 s2
  Ur str <- query_ s3 RawS.toStr
  Isl.pure (Ur str)

-- | Parse two ISL set strings, apply a binary predicate, return Bool.
binopBool
  :: (Isl.SetRef -> Isl.SetRef -> Bool)
  -> String -> String -> Bool
binopBool op str1 str2 = unsafePerformIO $ runIslT $ Isl.do
  s1 <- RawS.readFromStr str1
  Ur b <- Isl.queryM_ s1 (\ref1 -> Isl.do
    s2 <- RawS.readFromStr str2
    Ur r <- query_ s2 (\ref2 -> op ref1 ref2)
    Isl.pure (Ur r))
  Isl.pure (Ur b)

-- | Parse a single ISL set string, apply a unary predicate, return Bool.
unopSetBool
  :: (Isl.SetRef -> Bool)
  -> String -> Bool
unopSetBool op str = unsafePerformIO $ runIslT $ Isl.do
  s <- RawS.readFromStr str
  Ur b <- query_ s op
  Isl.pure (Ur b)

-- | Apply a map to a set (both as ISL strings), return the result string.
binopStringMap :: String -> String -> String
binopStringMap mapStr setStr = unsafePerformIO $ runIslT $ Isl.do
  m  <- RawM.readFromStr mapStr
  s  <- RawS.readFromStr setStr
  s' <- RawS.apply s m
  Ur str <- query_ s' RawS.toStr
  Isl.pure (Ur str)

-- | Apply a map to a set and check if the image is a subset of a target set.
applySubsetCheck :: String -> String -> String -> Bool
applySubsetCheck mapStr srcStr dstStr = unsafePerformIO $ runIslT $ Isl.do
  m    <- RawM.readFromStr mapStr
  src  <- RawS.readFromStr srcStr
  img  <- RawS.apply src m
  Ur b <- Isl.queryM_ img (\imgRef -> Isl.do
    dst <- RawS.readFromStr dstStr
    Ur r <- query_ dst (\dstRef -> RawS.isSubset imgRef dstRef)
    Isl.pure (Ur r))
  Isl.pure (Ur b)


-- ═══════════════════════════════════════════════════════════════════════
-- §6. Mirror functions — the pillar-1 catalog
-- ═══════════════════════════════════════════════════════════════════════

-- | Compute the union of two domains and yield a fresh reflected tag.
--
-- This is a one-sided mirror: the plugin doesn't currently expose an
-- 'IslUnionSet' type family rewriter, so there's no compile-time twin
-- to compare against.  See deviation D16 in the implementation log.
islUnion
  :: forall ps n d1 d2 r.
     ( KnownDom ps n d1
     , KnownDom ps n d2
     , KnownNat n
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => Proxy d1
  -> Proxy d2
  -> ( forall (d3 :: DomTag ps n). KnownDom ps n d3 => Proxy d3 -> r )
  -> r
islUnion _ _ k =
  let str1 = reflectDomString @ps @n @d1
      str2 = reflectDomString @ps @n @d2
      strRes = binopString RawS.union str1 str2
   in reifyDomFromString @ps @n strRes k

-- | Compute the intersection of two domains and yield a fresh
-- reflected tag.  This is the value-level mirror of the plugin's
-- 'IslIntersectSet' type family rewriter.
islIntersect
  :: forall ps n d1 d2 r.
     ( KnownDom ps n d1
     , KnownDom ps n d2
     , KnownNat n
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => Proxy d1
  -> Proxy d2
  -> ( forall (d3 :: DomTag ps n). KnownDom ps n d3 => Proxy d3 -> r )
  -> r
islIntersect _ _ k =
  let str1 = reflectDomString @ps @n @d1
      str2 = reflectDomString @ps @n @d2
      strRes = binopString RawS.intersect str1 str2
   in reifyDomFromString @ps @n strRes k

-- | Apply a runtime-built ISL map (given as an ISL string) to the set
-- labelled by @d@ and yield a fresh reflected tag for the image.
--
-- This is the value-level mirror of the plugin's 'Isl.TypeLevel.Constraint.IslApply'
-- type family rewriter.  The plugin rewrites @IslApply ps ni no mapCs
-- setCs@ by reifying @mapCs@ / @setCs@ to runtime ISL objects, calling
-- @isl_set_apply@, and lifting the result back to type-level syntax;
-- 'islApply' calls the same @isl_set_apply@ (via 'HMap.apply') on
-- runtime objects produced from the map string and the source
-- dictionary's 'reflectDomString'.  Two code paths, same C primitive,
-- same answer.
--
-- Unlike the plugin path, 'islApply' takes the map as a plain
-- ISL string rather than as a type-level constraint list — v6 does
-- not need type-level map tags because the only mirrors that consume
-- maps ('islApply' and 'islImageSubsetCheck') take them as strings
-- built either from 'IslMapToString' on a literal constraint list or
-- by value-level formatting inside a transform body.
islApply
  :: forall ps ni no dSrc r.
     ( KnownDom ps ni dSrc
     , KnownNat ni
     , KnownNat no
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => String            -- ^ the map's ISL text (runtime-built or 'symbolVal')
  -> Proxy dSrc        -- ^ the source domain dictionary
  -> ( forall (dDst :: DomTag ps no). KnownDom ps no dDst
        => Proxy dDst -> r )
  -> r
islApply mapStr _ k =
  let srcStr = reflectDomString @ps @ni @dSrc
      imgStr = binopStringMap mapStr srcStr
   in reifyDomFromString @ps @no imgStr k

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
  let ok = binopBool RawS.isSubset
             (reflectDomString @ps @n @d1)
             (reflectDomString @ps @n @d2)
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
      srcStr = reflectDomString @ps @ni @dSrc
      dstStr = reflectDomString @ps @no @dDst
      ok     = applySubsetCheck mapStr srcStr dstStr
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
  let srcStr = reflectDomString @ps @ni @dSrc
      dstStr = reflectDomString @ps @no @dDst
      ok     = applySubsetCheck mapStr srcStr dstStr
   in if ok then Just imgDict else Nothing
  where
    imgDict :: Dict (IslImageSubsetD ps ni no mapCs dSrc dDst)
    imgDict = unsafeCoerce (Dict :: Dict (() :: Constraint))

-- | Predicate mirror: is the set labelled by @d@ non-empty?  Used by
-- the v6 'tile2D' transform to detect "tile factor too large" (the
-- tiled domain has no integer points, so the recurrence never fires).
--
-- Returns a plain @'Bool'@ rather than a 'Maybe' 'Dict' because
-- non-emptiness is not itself a dispatch-class obligation in
-- "Alpha.Core"; the transform translates a 'False' result into
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
  not (unopSetBool RawS.isEmpty (reflectDomString @ps @n @d))


