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

-- | Acceptance tests for 'tile' at rank 3.  Exercises both full-rank
-- and partial-rank tiling on 'Examples.Const3D'.
-- Domain computed by ISL via 'IslPreimageMultiAff'.
module Examples.TiledConst3D
  ( runTileConst3DFull
  , runTileConst3DPartial
  , runTileConst3DFullDomain
  , runTileConst3DPartialDomain
  ) where

import Data.List (isInfixOf)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

import Alpha.Transform.Tile (tile, TransformError(..))
import Alpha.TypeLevel.Tiling (TileMapExprs)
import Examples.Const3D (const3D, CubeN3)
import Isl.TypeLevel.Constraint (IslPreimageMultiAff, IslToString)
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Reflection (DomTag(..), reflectDomString)


-- Full-rank preimage: [Just 2, Just 2, Just 2] on CubeN3
type FullTileMapExprs = TileMapExprs '[ 'Just 2, 'Just 2, 'Just 2]
  :: [TExpr '["N"] 6]
type FullPreimage = IslPreimageMultiAff '["N"] 6 3 FullTileMapExprs CubeN3

-- Partial preimage: [Just 2, Nothing, Just 2] on CubeN3
type PartialTileMapExprs = TileMapExprs '[ 'Just 2, 'Nothing, 'Just 2]
  :: [TExpr '["N"] 5]
type PartialPreimage = IslPreimageMultiAff '["N"] 5 3 PartialTileMapExprs CubeN3


-- | Full-rank tile: every dim split.
runTileConst3DFull :: Assertion
runTileConst3DFull =
  case tile "w" [Just 2, Just 2, Just 2] const3D of
    Left err -> assertFailure ("tile full failed: " ++ show err)
    Right _newSys -> pure ()

-- | Partial tile: middle dim left alone.
runTileConst3DPartial :: Assertion
runTileConst3DPartial =
  case tile "w" [Just 2, Nothing, Just 2] const3D of
    Left err -> assertFailure ("tile partial failed: " ++ show err)
    Right _newSys -> pure ()

-- | Full-rank domain content check.
runTileConst3DFullDomain :: Assertion
runTileConst3DFullDomain = do
  let domStr = reflectDomString @'["N"] @6 @('Literal FullPreimage)
  assertBool ("domain mentions N: " ++ domStr) ("N" `isInfixOf` domStr)
  assertBool ("domain has tile factor 2i0: " ++ domStr)
             ("2i0" `isInfixOf` domStr)

-- | Partial tile domain content check.
runTileConst3DPartialDomain :: Assertion
runTileConst3DPartialDomain = do
  let domStr = reflectDomString @'["N"] @5 @('Literal PartialPreimage)
  assertBool ("domain mentions N: " ++ domStr) ("N" `isInfixOf` domStr)
  assertBool ("domain has tile factor 2i0: " ++ domStr)
             ("2i0" `isInfixOf` domStr)
