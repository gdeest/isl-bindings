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
-- Each function here fabricates a proof (Dict or ':~:') via
-- @unsafeCoerce@.  Soundness follows from the defining equations
-- of the type families on concrete lists — see per-function comments.
--
-- Isolated here so that "Alpha.Core" itself is @unsafeCoerce@-free.
module Alpha.Core.Lemmas
  ( lookupReplaceDecl
  , replaceDeclList
  , definesAllReplace
  , replaceDeclConcat
    -- * Variable introduction axioms
  , definesAllIntroduce
  , introduceDecls
  , introduceEqList
  , lookupIntroduce
  , lookupIntroduceDecl
  ) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy)
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


-- ═══════════════════════════════════════════════════════════════════════
-- ReplaceDecl / Lookup axioms
-- ═══════════════════════════════════════════════════════════════════════

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
-- Variable introduction axioms
-- ═══════════════════════════════════════════════════════════════════════

-- | 'DefinesAllExactlyOnce' extends when adding a fresh local
-- with its matching equation.
--
-- Soundness: @newName@ is fresh (not in @outputs ++ locals@), so
-- @CountName newName (outputs ++ (newDecl ': locals)) = 1@ (the new
-- entry) and @RemoveName@ leaves @outputs ++ locals@ for the rest.
-- Then @defined@ recurses on the original @DefinesAllExactlyOnce@.
definesAllIntroduce
  :: forall (ps :: [Symbol]) (newDecl :: VarDecl ps)
            (newName :: Symbol) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) (defined :: [Symbol]).
     DefinesAllExactlyOnce (outputs ++ locals) defined
  => Dict (DefinesAllExactlyOnce
             (outputs ++ (newDecl ': locals))
             (newName ': defined))
definesAllIntroduce = unsafeCoerce (Dict :: Dict (() :: Constraint))
{-# INLINE definesAllIntroduce #-}

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
  :: forall (ps :: [Symbol]) (newDecl :: VarDecl ps)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]) (defined :: [Symbol]).
     EqList ps (inputs ++ (outputs ++ locals)) defined
  -> EqList ps (inputs ++ (outputs ++ (newDecl ': locals))) defined
introduceEqList = unsafeCoerce
{-# INLINE introduceEqList #-}

-- | Lookup the freshly-introduced variable in the augmented decl list.
-- Soundness: @newDecl@ has @DeclName newDecl ~ proxyName@ and is
-- consed onto locals. Since @proxyName@ is fresh, @Lookup@ traverses
-- inputs and outputs without matching, then finds @newDecl@ at the
-- head of the new locals.
lookupIntroduce
  :: forall (ps :: [Symbol]) (proxyName :: Symbol)
            (newDecl :: VarDecl ps)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps]).
     Dict (Lookup proxyName (inputs ++ (outputs ++ (newDecl ': locals))) ~ newDecl)
lookupIntroduce = unsafeCoerce (Dict :: Dict (() :: Constraint))
{-# INLINE lookupIntroduce #-}

-- | Axiom: 'Lookup' over introduce (cons-new-local).
--
-- Same dispatch as 'lookupReplaceDecl': if @name ~ proxy@, Lookup
-- finds the freshly-introduced @newDecl@; otherwise Lookup returns
-- the same result as in the original decl list.
--
-- Precondition: @proxy@ is fresh in @oldDecls@.
lookupIntroduceDecl
  :: forall (ps :: [Symbol]) (proxy :: Symbol) (name :: Symbol)
            (newDecl :: VarDecl ps)
            (oldDecls :: [VarDecl ps]) (newDecls :: [VarDecl ps]).
     ( KnownSymbol proxy, KnownSymbol name )
  => Proxy proxy -> Proxy name
  -> Either
       ( name :~: proxy
       , Dict (Lookup name newDecls ~ newDecl) )
       ( Dict (Lookup name newDecls ~ Lookup name oldDecls) )
lookupIntroduceDecl proxy name =
  case sameSymbol name proxy of
    Just Refl -> Left  (Refl, unsafeCoerce (Dict :: Dict (() :: Constraint)))
    Nothing   -> Right (unsafeCoerce (Dict :: Dict (() :: Constraint)))
{-# INLINE lookupIntroduceDecl #-}
