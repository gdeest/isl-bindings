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
-- = What this is *not*
--
-- The transform does NOT rebuild the 'System' value with a new
-- phantom type for the input variable's declared domain — it is a
-- pure validation+observation gate.  The continuation receives the
-- *original* 'System' unchanged but can observe the new domain via
-- 'domToString' on the new tag.
module Alpha.Transform.Domain
  ( -- * Errors (re-exported from "Alpha.Transform.Types")
    TransformError(..)
    -- * The transform
  , replaceInputDomain
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, symbolVal)

import Alpha.Surface.Core (System, VarDecl)
import Alpha.Transform.Types (TransformError(..))
import Isl.TypeLevel.Constraint (TConstraint)
import Isl.Typed.Params (KnownSymbols, Length)
import Isl.TypeLevel.Reflection
  ( Dict(..)
  , DomTag
  , KnownDom
  , islSubsetCheck
  , domToString
  )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. The transform
-- ═══════════════════════════════════════════════════════════════════════

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
-- Returns 'Left' if the runtime check fails, or 'Right' with the
-- continuation's result if the check passes.
replaceInputDomain
  :: forall (name :: Symbol) (ps :: [Symbol]) (n :: Nat)
            (pctx :: [TConstraint ps 0])
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
  -> System ps pctx inputs outputs locals
  -- NOTE: replaceInputDomain's runtime mirror @islSubsetCheck@ does not
  -- consume the system's pctx — a follow-up should thread pctx through
  -- the check so the shrink validates under the system's preconditions.
  -> IO r
  -> IO (Either TransformError r)
replaceInputDomain _ _ _ _sys k =
  -- Runtime obligation check via the mirror function.  The mirror
  -- returns @Just Dict@ on success; the validation gate does not
  -- construct new AST nodes, so we do not need the evidence in scope
  -- past the pattern match — we just use the 'Just'/'Nothing' shape
  -- as a two-state gate.
  case islSubsetCheck @ps @n @expectedImageDom @newD Proxy Proxy of
    Just Dict -> Right <$> k
    Nothing ->
      let nameStr  = symbolVal (Proxy @name)
          imgStr   = domToString @ps @n @expectedImageDom
          newStr   = domToString @ps @n @newD
       in pure (Left (DomainShrinkUnsafe nameStr imgStr newStr))
