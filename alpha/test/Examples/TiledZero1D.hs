{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin=Isl.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Acceptance tests for the constructive 'tile' and 'reindex'
-- transforms on the minimal rank-1 'Examples.Zero1D' shape.
--
-- The new domain is computed by ISL via 'IslPreimageMultiAff' —
-- no hand-rolled type families.
module Examples.TiledZero1D
  ( runTileZero1D
  , runTileDomainCheck
  , runReindexZero1D
  ) where

import Data.List (isInfixOf)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

import Alpha.Transform.Reindex (reindex)
import Alpha.Transform.Tile (tile)
import Examples.Zero1D (zero1D, LineN)
import Isl.TypeLevel.Constraint (IslPreimageMultiAff, IslToString)
import Isl.TypeLevel.Expr (D, TExpr(..), Z(..))
import Isl.TypeLevel.Reflection (DomTag(..), reflectDomString)


-- The tiling multi-aff: [ti, i] -> [4*ti + i]
type TileMapExprs = '[ 'TAdd ('TMul ('Pos 4) ('TDim (D 0))) ('TDim (D 1)) ]
  :: [TExpr '["N"] 2]

-- The preimage domain computed by ISL
type PreimageDomain = IslPreimageMultiAff '["N"] 2 1 TileMapExprs LineN


-- | Positive: @tile \"y\" '[Just 4] zero1D@ succeeds.
-- The walker constructively rebuilds the System (Const body is
-- domain-polymorphic).  The new domain is ISL-computed.
runTileZero1D :: Assertion
runTileZero1D =
  case tile "y" [Just 4] zero1D of
    Left err ->
      assertFailure ("tile \"y\" '[Just 4] zero1D failed: " ++ show err)
    Right _newSys ->
      pure ()


-- | Verify the ISL-computed tiled domain has the right shape.
-- Uses reflectDomString on the IslPreimageMultiAff result.
runTileDomainCheck :: Assertion
runTileDomainCheck = do
  let domStr = reflectDomString @'["N"] @2 @('Literal PreimageDomain)
  -- Expected: {[i0, i1] : 4*i0 + i1 >= 0, 4*i0 + i1 <= N - 1}
  assertBool ("domain mentions N: " ++ domStr)
             ("N" `isInfixOf` domStr)
  assertBool ("domain has tile factor 4i0: " ++ domStr)
             ("4i0" `isInfixOf` domStr)


-- | End-to-end reindex with explicit multi-aff.
runReindexZero1D :: Assertion
runReindexZero1D =
  case reindex "y" 2 (type TileMapExprs) zero1D of
    Left err ->
      assertFailure ("reindex zero1D failed: " ++ show err)
    Right _newSys ->
      pure ()
