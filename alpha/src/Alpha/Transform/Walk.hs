{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Shared ISL map composition for Alpha transforms.
--
-- Extracted from 'Alpha.Transform.Reindex' so that both 'reindex'
-- and 'introduce' (and future transforms) reuse the same verified
-- map-composition logic.
module Alpha.Transform.Walk
  ( composeAccess
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal, type (+))
import System.IO.Unsafe (unsafePerformIO)

import Isl.Typed.Constraints (Conjunction(..), MapIx, Constraint)
import qualified Isl.Typed.Constraints as TC
import Isl.Monad (runIslT, Ur(..))
import Isl.Linear (queryM_, urWrap)
import qualified Isl.Linear as Isl
import Isl.Typed.Params (KnownSymbols, Length)
import qualified Isl.Map as RawM
import qualified Isl.Set as RawS
import Isl.TypeLevel.Constraint (TConstraint)
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Sing
  ( KnownConstraints(..), KnownExprs(..), SBasicMap(..), SMultiAff(..)
  , evalSBasicMap, evalSMultiAff
  )


-- ═══════════════════════════════════════════════════════════════════════
-- §1. Shared map composition
-- ═══════════════════════════════════════════════════════════════════════

-- | Compose an access map with a multi-aff transform map via ISL.
--
-- Given old dep map @(ni → oldNo)@ from KnownConstraints and a
-- multi-aff @(newNo → oldNo)@ from KnownExprs, computes:
--
--   @old_map ; reverse(multi_aff) = (ni → newNo)@
--
-- Then verifies: image of source domain under composed map ⊆ dest domain.
--
-- Returns the composed map's ISL string and constraint list, or
-- 'Nothing' if the image-subset check fails.
composeAccess
  :: forall ps (ni :: Nat) (oldNo :: Nat) (newNo :: Nat)
            (mapCs :: [TConstraint ps (ni + oldNo)])
            (mapExprs :: [TExpr ps newNo]).
     ( KnownNat ni, KnownNat oldNo, KnownNat newNo
     , KnownSymbols ps, KnownNat (Length ps)
     , KnownConstraints ps (ni + oldNo) mapCs
     , KnownExprs ps newNo mapExprs
     )
  => String  -- ^ source domain ISL string (ni-dim)
  -> String  -- ^ destination domain ISL string (newNo-dim)
  -> Maybe (String, [Constraint MapIx])
composeAccess srcStr dstStr = unsafePerformIO $ runIslT $ Isl.do
  oldMap <- evalSBasicMap @ps @ni @oldNo
              (MkSBasicMap (knownConstraints @ps @(ni + oldNo) @mapCs))
  reindexMA <- evalSMultiAff @ps @newNo @oldNo
                 (MkSMultiAff (knownExprs @ps @newNo @mapExprs))
  fwdMap    <- RawM.fromMultiAff reindexMA
  revMap    <- RawM.reverse fwdMap
  composed  <- RawM.applyRange oldMap revMap
  let nIn = fromIntegral (natVal (Proxy @ni))
      nNewNo = fromIntegral (natVal (Proxy @newNo))
      nParams = fromIntegral (natVal (Proxy @(Length ps)))
  let !(composed1, composed2) = Isl.dup composed
  Ur (composedStr, constrs) <- queryM_ composed1 (\composedRef -> Isl.do
    let !str = RawM.toStr composedRef
    conjs <- TC.decomposeMap nIn nNewNo nParams composedRef
    Ur cs <- urWrap conjs
    let constrs' = case cs of
          [Conjunction c] -> c
          _ -> error "composeAccess: non-basic composed map"
    Isl.pure (Ur (str, constrs')))
  src <- RawS.readFromStr srcStr
  img <- RawS.apply src composed2
  dst <- RawS.readFromStr dstStr
  Ur b <- queryM_ img (\imgRef ->
    Isl.query_ dst (\dstRef -> RawS.isSubset imgRef dstRef))
  Isl.pure (Ur (if b then Just (composedStr, constrs) else Nothing))
