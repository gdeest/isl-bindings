-- | Shared types for the @Alpha.Transform.*@ family.
--
-- Transforms live in @Alpha.Transform.Domain@ (the reflected-route
-- validation gate) and @Alpha.Transform.Loop@ ('tile2D' and friends),
-- and all of them return the same 'TransformError' shape on failure.
-- Centralising the error type in a dedicated module keeps the
-- transform modules from cross-importing each other for one data
-- type.
--
-- === Narrowing the ambient domain
--
-- Transforms that need to narrow an expression's ambient domain @d@
-- should /wrap/ in a @'Alpha.Surface.Core.Restrict'@ node — its
-- 'IslSubsetD' obligation @dOuter ⊆ dInner@ holds trivially for any
-- narrowing — rather than rewrite the phantom by constructor-dispatched
-- recursion.  See 'Alpha.Transform.Restrict.restrict'.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Alpha.Transform.Types
  ( TransformError(..)
    -- * Proof monad
  , Proof
  , requireJust
  , requireC
  , internalError
  ) where

import Isl.TypeLevel.Reflection (Dict(..))

-- | Structured error reported by Alpha transforms when a runtime
-- obligation check fails.
--
-- Each constructor carries enough information to print a diagnostic
-- (names, ISL strings) so a failing transform can explain *what* it
-- refused and *why* without the caller having to cross-reference
-- anything.
data TransformError
  = -- | The new declared domain does not contain an expected access
    -- image — replacing the variable's domain would invalidate at
    -- least one read access.  Carries the variable name, the expected
    -- access image's ISL string, and the proposed new domain's ISL
    -- string.
    --
    -- Surfaced by @replaceInputDomain@.
    DomainShrinkUnsafe !String !String !String
  | -- | A runtime-built domain has no integer points (e.g., a tile
    -- factor chosen larger than the ambient parameter makes the
    -- whole tiled domain empty at the concrete parameter value).
    -- Carries the variable name and the empty domain's ISL string.
    --
    -- Surfaced by @tile2D@.
    EmptyResultDomain !String !String
  | -- | A rewritten access's image in the target variable's declared
    -- domain failed the runtime @'islImageSubsetCheck'@ mirror.
    -- Carries the access label (e.g., @"A"@), the access map's ISL
    -- string, and the target domain's ISL string.
    --
    -- Surfaced by @tile2D@.
    ImageOutOfBounds !String !String !String
  deriving (Show, Eq)


-- ═══════════════════════════════════════════════════════════════════════
-- Proof monad — Either TransformError with Maybe-bridge combinators
-- ═══════════════════════════════════════════════════════════════════════

-- | Walker result type: either a 'TransformError' or the rewritten value.
-- Transform walkers chain ISL obligations via @do@-notation over 'Proof'.
type Proof = Either TransformError

-- | Promote a 'Maybe' into 'Proof', tagging 'Nothing' with a 'TransformError'.
requireJust :: TransformError -> Maybe a -> Proof a
requireJust err = maybe (Left err) Right

-- | CPS-style: turn a @Maybe (Dict c)@ obligation into an implicit
-- constraint scope.  On 'Just Dict', the continuation runs with @c@
-- in scope; on 'Nothing', the walker short-circuits with the given
-- 'TransformError'.
--
-- Prefer 'requireC' over @Dict <- ...@ in do-notation: the continuation
-- is plainly @(c => Proof r)@, which makes the constraint-introduction
-- intent visible without a stray 'Dict' binding in the walker body.
requireC
  :: TransformError
  -> Maybe (Dict c)
  -> (c => Proof r)
  -> Proof r
requireC _   (Just Dict) k = k
requireC err Nothing     _ = Left err

-- | A branch the type system cannot rule out but the walker invariant does.
-- Reserved for impossibilities like symbolVal/sameSymbol disagreement.
internalError :: String -> a
internalError msg = error ("internal invariant: " ++ msg)
