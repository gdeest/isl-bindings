{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

-- | Type-level infrastructure for the Alpha tile transform.
--
-- Most of the tile computation is now done by ISL via the plugin's
-- 'IslPreimageMultiAff'.  This module provides:
--
--   * 'TileMapExprs' — generates the forward multi-aff for strip-mine
--     tiling (new coords → old coords), consumed by 'reindex'
--   * 'Length', 'CountJust' — list helpers for computing the new rank
--   * Re-exports of 'ReplaceDecl', 'lookupReplaceDecl' from "Alpha.Core"
module Alpha.TypeLevel.Tiling
  ( -- * Decls list substitution (re-exported from Alpha.Core)
    ReplaceDecl
  , ReplaceDeclStep
  , lookupReplaceDecl
    -- * List helpers
  , Length
  , CountJust
    -- * Tile multi-aff (forward map: new → old coords)
  , TileMapExprs
  , TileMapExprsAt
  ) where

import GHC.TypeLits ( Nat, type (+) )

import Alpha.Core (ReplaceDecl, ReplaceDeclStep, lookupReplaceDecl)
import Isl.TypeLevel.Expr (Idx(..), TExpr(..), Z(..))


-- ═══════════════════════════════════════════════════════════════════════
-- §1. List helpers
-- ═══════════════════════════════════════════════════════════════════════

-- | Kind-polymorphic type-level list length.
type family Length (xs :: [k]) :: Nat where
  Length '[]       = 0
  Length (_ ': xs) = 1 + Length xs

-- | Number of 'Just' entries in a type-level @[Maybe k]@.
type family CountJust (xs :: [Maybe k]) :: Nat where
  CountJust '[]               = 0
  CountJust ('Just  _ ': xs)  = 1 + CountJust xs
  CountJust ('Nothing ': xs)  = CountJust xs


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Tile forward multi-aff (new coords → old coords)
-- ═══════════════════════════════��═══════════════════════════════════════

-- | Generate the forward multi-aff for strip-mine tiling.
--
-- For factors @'[Just 4, Nothing, Just 2]@ on a 3-dim variable:
--
--   * New coord layout (interleaved): @[t₀, d₀, d₁, t₂, d₂]@ (5 dims)
--   * Multi-aff: @[4*t₀ + d₀, d₁, 2*t₂ + d₂]@ (3 outputs)
--
-- Each @Just t@ at position @k@ contributes @t * D_{pos} + D_{pos+1}@
-- and advances @pos@ by 2.  Each @Nothing@ contributes @D_{pos}@ and
-- advances by 1.
type TileMapExprs (factors :: [Maybe Nat]) = TileMapExprsAt 0 factors

type family TileMapExprsAt (pos :: Nat) (factors :: [Maybe Nat])
          :: [TExpr ps n] where
  TileMapExprsAt _ '[] = '[]
  TileMapExprsAt pos ('Just t ': rest) =
    'TAdd ('TMul ('Pos t) ('TDim ('MkIdx pos))) ('TDim ('MkIdx (pos + 1)))
    ': TileMapExprsAt (pos + 2) rest
  TileMapExprsAt pos ('Nothing ': rest) =
    'TDim ('MkIdx pos)
    ': TileMapExprsAt (pos + 1) rest
