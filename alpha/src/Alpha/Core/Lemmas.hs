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
  ) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits (KnownSymbol, Symbol, sameSymbol)
import Unsafe.Coerce (unsafeCoerce)

import Isl.TypeLevel.Reflection (Dict(..))

import Alpha.Core
  ( VarDecl, Lookup, ReplaceDecl
  , DeclList
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
