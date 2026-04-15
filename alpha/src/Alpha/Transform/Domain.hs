{-# LANGUAGE AllowAmbiguousTypes #-}
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

-- | Transforms that replace a variable's declared domain with a new
-- (possibly reflected) tag.
--
-- See @doc/alpha-design.md@ §6 (Pillar 3: plain-function transforms)
-- and the v1 milestone in @doc/alpha-implementation.md@ for the
-- design intent and the v1 simplifications.
--
-- = What this module provides
--
-- A single transform function 'replaceInputDomain'.  It is a CPS-style
-- validation gate: take a 'System', a variable name, an expected
-- "access image" (the polyhedron the equation bodies actually read
-- from), and a new declared domain tag.  Verify at runtime via the
-- 'islSubsetCheck' mirror that the new declared domain still contains
-- the access image.  If yes, run the user-supplied continuation; if
-- no, return 'Left' with a structured 'TransformError' identifying
-- which constraint failed.
--
-- = Trust base
--
-- Zero 'unsafeCoerce' in this file.  All ISL operations are performed
-- by the mirror functions in 'Isl.TypeLevel.Reflection', whose own
-- trust base is documented at the top of that module.
--
-- = What this is *not* (yet)
--
-- The transform does NOT actually rebuild the 'System' value with a
-- new phantom type for the input variable's declared domain.  The
-- design's full Pillar-3 transform would do that via type-level
-- surgery on 'inputs' / 'EqList'; v1 sidesteps this complexity by
-- having the transform be a pure validation+observation gate.  See
-- deviation D10 in the implementation log for the rationale.
--
-- The continuation receives the *original* System unchanged.  It can
-- still observe the new domain via 'reflectDomString' on the new tag,
-- which is enough to demonstrate the reflected route end-to-end (and
-- enough for the v1 acceptance test).  Pillar 3's full type-level
-- replacement is deferred to v5+.
module Alpha.Transform.Domain
  ( -- * Errors (re-exported from "Alpha.Transform.Types")
    TransformError(..)
    -- * The transform
  , replaceInputDomain
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, symbolVal)

import Alpha.Core (System, VarDecl)
import Alpha.Transform.Types (TransformError(..))
import Isl.Typed.Params (KnownSymbols, Length)
import Isl.TypeLevel.Reflection
  ( Dict(..)
  , DomTag
  , KnownDom
  , islSubsetCheck
  , reflectDomString
  )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. The transform
-- ═══════════════════════════════════════════════════════════════════════
--
-- The 'TransformError' type lives in "Alpha.Transform.Types" as of v6;
-- it is re-exported above so v1 callers do not break.

-- | Validate that replacing the named input variable's declared domain
-- with @newD@ would not invalidate any reads from that variable, then
-- run the continuation.
--
-- The "expected access image" parameter @expectedImageDom@ is the
-- polyhedron the program's bodies actually read from (the image of
-- their iteration domains under the access maps to this variable).
-- For matmul accessing @A@ from a 3D body via @(i,j,k) → (i,k)@, the
-- expected image is @SquareN@.
--
-- Why we take the image as a parameter rather than computing it: in
-- v1, walking the equation bodies to extract access maps would
-- require pattern-matching on the GADT, which is straightforward but
-- adds substantial code for no v1-acceptance value.  The user-as-
-- compiler-author supplies the image directly, demonstrating the
-- check without the walking.  Recorded as deviation D18.
--
-- The continuation has access to:
--
--   * The original 'System' value (unchanged — there is no new System
--     value in v1; see deviation D10).
--   * The 'KnownDom' dictionary for @newD@ via 'reflectDomString',
--     which is what the v1 acceptance test verifies.
--
-- Returns 'Left' if the runtime check fails, or 'Right' with the
-- continuation's result if the check passes.
replaceInputDomain
  :: forall (name :: Symbol) (ps :: [Symbol]) (n :: Nat)
            (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
            (locals :: [VarDecl ps])
            (expectedImageDom :: DomTag ps n) (newD :: DomTag ps n) r.
     ( KnownSymbol name
     , KnownDom ps n expectedImageDom
     , KnownDom ps n newD
     , KnownNat n
     , KnownSymbols ps
     , KnownNat (Length ps)
     )
  => Proxy name
  -> Proxy expectedImageDom
  -> Proxy newD
  -> System ps inputs outputs locals
  -> IO r
  -> IO (Either TransformError r)
replaceInputDomain _ _ _ _sys k =
  -- Runtime obligation check via the mirror function.  The mirror
  -- returns @Just Dict@ on success; v1's validation gate does not
  -- construct new AST nodes, so we do not need the evidence in scope
  -- past the pattern match — we just use the 'Just'/'Nothing' shape
  -- as a two-state gate.
  case islSubsetCheck @ps @n @expectedImageDom @newD Proxy Proxy of
    Just Dict -> Right <$> k
    Nothing ->
      let nameStr  = symbolVal (Proxy @name)
          imgStr   = reflectDomString @ps @n @expectedImageDom
          newStr   = reflectDomString @ps @n @newD
       in pure (Left (DomainShrinkUnsafe nameStr imgStr newStr))
