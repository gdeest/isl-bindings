-- | Shared types for the @Alpha.Transform.*@ family.
--
-- Transforms live in @Alpha.Transform.Domain@ (v1 — the reflected-route
-- validation gate) and @Alpha.Transform.Loop@ (v6 — 'tile2D' and friends),
-- and all of them return the same 'TransformError' shape on failure.
-- Centralising the error type in a dedicated module keeps the transform
-- modules from cross-importing each other for one data type.
--
-- === Phantom polarity — read this before writing a new transform
--
-- An @'Alpha.Core.Expr' ps decls n d a@ carries its ambient domain @d@
-- as a phantom.  Transforms that need to narrow @d@ (e.g.
-- @d ↦ d ∩ d'@) cannot in general do so by constructor-dispatched
-- recursion, because two constructors have the /wrong polarity/ for
-- phantom rewriting:
--
--  * 'Alpha.Core.Reduce' — @d@ is the /result/ domain; the obligation
--    @image(dBody, projCs) ⊆ d@ tightens as @d@ narrows, and is
--    generally false under narrowing because a reduction's image
--    typically covers the whole result domain.
--
--  * 'Alpha.Core.Case' — branches partition @d@; their partition
--    witness @'Isl.Typed.Constraints.IslPartitionsD'@ may no longer
--    cover a narrower @d'@ (coverage is a property of @d@).
--
-- The sound, universal move is to /wrap/ the expression in a
-- @'Alpha.Core.Dep' \@identityMap@ node — 'Dep''s obligation
-- @image(dOuter, mapCs) ⊆ dInner@ has the correct polarity (weaker
-- source → smaller image), and under the identity map reduces to
-- @dOuter ⊆ dInner@, which holds trivially for any narrowing.  See
-- 'Alpha.Transform.Weaken.weakenExpr'.
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
    -- Shipped in v1 by @replaceInputDomain@.
    DomainShrinkUnsafe !String !String !String
  | -- | A runtime-built domain has no integer points (e.g., a tile
    -- factor chosen larger than the ambient parameter makes the
    -- whole tiled domain empty at the concrete parameter value).
    -- Carries the variable name and the empty domain's ISL string.
    --
    -- Shipped in v6 by @tile2D@.
    EmptyResultDomain !String !String
  | -- | A rewritten access's image in the target variable's declared
    -- domain failed the runtime @'islImageSubsetCheck'@ mirror.
    -- Carries the access label (e.g., @"A"@), the access map's ISL
    -- string, and the target domain's ISL string.
    --
    -- Shipped in v6 by @tile2D@.
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
