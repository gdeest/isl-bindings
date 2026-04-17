{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Unsafe axioms for 'ReplaceDecl' / 'Lookup' interaction.
--
-- Each axiom fabricates a proof via @unsafeCoerce@ and exposes it
-- through a CPS continuation of shape @(c => r) -> r@ — the evidence
-- never escapes as a first-class 'Dict', so transform developers work
-- entirely in terms of constraints scoped over their walker branches.
--
-- Soundness follows from the defining equations of the type families
-- on concrete lists — see per-function comments.
module Alpha.Core.Lemmas
  ( -- * Replace-decl axioms (CPS)
    withReplaceDecl
  , withDefinesAllReplace
  , withReplaceDeclConcat
  , replaceDeclList
    -- * Introduce axioms (CPS)
  , withIntroduceDecl
  , withIntroduce
  , withDefinesAllIntroduce
  , introduceDecls
  , introduceEqList
  ) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits (KnownSymbol, Symbol, sameSymbol)
import Unsafe.Coerce (unsafeCoerce)

import Isl.TypeLevel.Reflection (Dict(..))

import Alpha.Core
  ( VarDecl, Lookup, ReplaceDecl
  , DeclList((:>)), Decl, Decls(..)
  , EqList
  , DefinesAllExactlyOnce
  , type (++)
  )
import Isl.TypeLevel.Constraint (TConstraint)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Replace-decl axioms
-- ═══════════════════════════════════════════════════════════════════════

-- | CPS dispatch over 'ReplaceDecl' / 'Lookup'.
--
-- @
-- name ~ target  ==>  Lookup name (ReplaceDecl target new decls) ~ new
-- name ≠ target  ==>  Lookup name (ReplaceDecl target new decls) ~ Lookup name decls
-- @
--
-- Dispatches via 'sameSymbol'; each branch brings the appropriate
-- equality into the continuation's constraint context.  The @hit@
-- branch additionally receives @name ~ target@.  Argument order
-- mirrors 'Either' (miss first, hit second).
--
-- Soundness: follows from the equations of 'ReplaceDecl' + 'Lookup'
-- on concrete lists, lifted to abstract lists via a single
-- @unsafeCoerce@.
withReplaceDecl
  :: forall (ps :: [Symbol]) (target :: Symbol) (name :: Symbol)
            (new :: VarDecl ps) (decls :: [VarDecl ps]) r.
     ( KnownSymbol target, KnownSymbol name )
  => Proxy target -> Proxy name
  -> ((Lookup name (ReplaceDecl target new decls)
         ~ Lookup name decls) => r)
  -> (( name ~ target
      , Lookup name (ReplaceDecl target new decls) ~ new
      ) => r)
  -> r
withReplaceDecl _ _ onMiss onHit =
  case sameSymbol (Proxy @name) (Proxy @target) of
    Nothing ->
      case unsafeCoerce (Dict :: Dict (() :: Constraint))
             :: Dict (Lookup name (ReplaceDecl target new decls)
                        ~ Lookup name decls) of
        Dict -> onMiss
    Just Refl ->
      case unsafeCoerce (Dict :: Dict (() :: Constraint))
             :: Dict (Lookup name (ReplaceDecl target new decls) ~ new) of
        Dict -> onHit

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
-- (it only examines 'DeclName's, which are preserved).  Supplies
-- the preserved evidence to the continuation.
withDefinesAllReplace
  :: forall (target :: Symbol) (ps :: [Symbol])
            (newDecl :: VarDecl ps) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) (defined :: [Symbol]) r.
     DefinesAllExactlyOnce (outputs ++ locals) defined
  => ( DefinesAllExactlyOnce
         (ReplaceDecl target newDecl outputs
          ++ ReplaceDecl target newDecl locals)
         defined
       => r)
  -> r
withDefinesAllReplace k =
  case unsafeCoerce (Dict :: Dict (() :: Constraint))
         :: Dict (DefinesAllExactlyOnce
                    (ReplaceDecl target newDecl outputs
                     ++ ReplaceDecl target newDecl locals)
                    defined) of
    Dict -> k
{-# INLINE withDefinesAllReplace #-}

-- | 'ReplaceDecl' distributes over '++' when the target appears
-- exactly once in @outputs ++ locals@ and not in @inputs@.
-- Supplies the type-level equality to the continuation.
withReplaceDeclConcat
  :: forall (target :: Symbol) (ps :: [Symbol])
            (newDecl :: VarDecl ps)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) r.
     ( ( inputs ++ (ReplaceDecl target newDecl outputs
                    ++ ReplaceDecl target newDecl locals) )
       ~ ReplaceDecl target newDecl (inputs ++ (outputs ++ locals))
       => r)
  -> r
withReplaceDeclConcat k =
  case unsafeCoerce Refl
         :: (inputs ++ (ReplaceDecl target newDecl outputs
                        ++ ReplaceDecl target newDecl locals))
            :~:
            ReplaceDecl target newDecl (inputs ++ (outputs ++ locals)) of
    Refl -> k
{-# INLINE withReplaceDeclConcat #-}


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Variable introduction axioms
-- ═══════════════════════════════════════════════════════════════════════

-- | 'DefinesAllExactlyOnce' extends when adding a fresh local with
-- its matching equation.
--
-- Soundness: @newName@ is fresh (not in @outputs ++ locals@), so
-- @CountName newName (outputs ++ (newDecl ': locals)) = 1@ (the new
-- entry) and @RemoveName@ leaves @outputs ++ locals@ for the rest.
-- Then @defined@ recurses on the original @DefinesAllExactlyOnce@.
withDefinesAllIntroduce
  :: forall (ps :: [Symbol]) (newDecl :: VarDecl ps)
            (newName :: Symbol) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) (defined :: [Symbol]) r.
     DefinesAllExactlyOnce (outputs ++ locals) defined
  => ( DefinesAllExactlyOnce
         (outputs ++ (newDecl ': locals))
         (newName ': defined)
       => r)
  -> r
withDefinesAllIntroduce k =
  case unsafeCoerce (Dict :: Dict (() :: Constraint))
         :: Dict (DefinesAllExactlyOnce
                    (outputs ++ (newDecl ': locals))
                    (newName ': defined)) of
    Dict -> k
{-# INLINE withDefinesAllIntroduce #-}

-- | Cons a fresh local onto the 'Decls' record.
-- Runtime: just widens the phantom type on @dLocals@.
introduceDecls
  :: forall (ps :: [Symbol]) (newDecl :: VarDecl ps)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]).
     Decl ps newDecl
  -> Decls ps inputs outputs locals
  -> Decls ps inputs outputs (newDecl ': locals)
introduceDecls newD (Decls ins outs locs) =
  unsafeCoerce (Decls ins outs (newD Alpha.Core.:> locs))
{-# INLINE introduceDecls #-}

-- | Transport an 'EqList' from old decl environment to augmented one.
-- Sound when @newDecl@ has a fresh name: 'Lookup' for every existing
-- name gives the same result in both environments.
introduceEqList
  :: forall (ps :: [Symbol]) (pctx :: [TConstraint ps 0])
            (newDecl :: VarDecl ps)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) (defined :: [Symbol]).
     EqList ps pctx (inputs ++ (outputs ++ locals)) defined
  -> EqList ps pctx (inputs ++ (outputs ++ (newDecl ': locals))) defined
introduceEqList = unsafeCoerce
{-# INLINE introduceEqList #-}

-- | Supply the freshly-introduced variable's 'Lookup' equality
-- to the continuation.
--
-- Soundness: @newDecl@ has @DeclName newDecl ~ proxyName@ and is
-- consed onto locals.  Since @proxyName@ is fresh, @Lookup@
-- traverses inputs and outputs without matching, then finds
-- @newDecl@ at the head of the new locals.
withIntroduce
  :: forall (ps :: [Symbol]) (proxyName :: Symbol)
            (newDecl :: VarDecl ps)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) r.
     ( Lookup proxyName (inputs ++ (outputs ++ (newDecl ': locals)))
       ~ newDecl
       => r)
  -> r
withIntroduce k =
  case unsafeCoerce (Dict :: Dict (() :: Constraint))
         :: Dict (Lookup proxyName
                    (inputs ++ (outputs ++ (newDecl ': locals)))
                  ~ newDecl) of
    Dict -> k
{-# INLINE withIntroduce #-}

-- | CPS dispatch over introduce (cons-new-local).
--
-- Same shape as 'withReplaceDecl': if @name ~ proxy@, 'Lookup' finds
-- the freshly-introduced @newDecl@; otherwise it returns the same
-- result as in the original decl list.
--
-- Precondition: @proxy@ is fresh in @oldDecls@.
withIntroduceDecl
  :: forall (ps :: [Symbol]) (proxy :: Symbol) (name :: Symbol)
            (newDecl :: VarDecl ps)
            (oldDecls :: [VarDecl ps]) (newDecls :: [VarDecl ps]) r.
     ( KnownSymbol proxy, KnownSymbol name )
  => Proxy proxy -> Proxy name
  -> ((Lookup name newDecls ~ Lookup name oldDecls) => r)
  -> (( name ~ proxy
      , Lookup name newDecls ~ newDecl
      ) => r)
  -> r
withIntroduceDecl _ _ onMiss onHit =
  case sameSymbol (Proxy @name) (Proxy @proxy) of
    Nothing ->
      case unsafeCoerce (Dict :: Dict (() :: Constraint))
             :: Dict (Lookup name newDecls ~ Lookup name oldDecls) of
        Dict -> onMiss
    Just Refl ->
      case unsafeCoerce (Dict :: Dict (() :: Constraint))
             :: Dict (Lookup name newDecls ~ newDecl) of
        Dict -> onHit
{-# INLINE withIntroduceDecl #-}
