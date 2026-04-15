{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

-- | Tiling transform for Alpha systems, built on 'reindex'.
--
-- @tile target factors sys@ is sugar for 'reindex' with the
-- strip-mining multi-aff derived from @factors@.  The new domain is
-- computed by ISL via 'IslPreimageMultiAff' — no hand-rolled
-- type families needed.
module Alpha.Transform.Tile
  ( tile
  , TransformError(..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits
  ( KnownNat, KnownSymbol, Nat, Symbol, type (+) )

import Alpha.Core
import Alpha.Transform.Reindex (reindex, TransformError(..))
import Alpha.TypeLevel.Tiling
  ( CountJust
  , Length
  , ReplaceDecl
  , TileMapExprs
  )
import Isl.Typed.Params (KnownSymbols)
import Isl.Typed.Params qualified as P
import Isl.TypeLevel.Constraint
  ( IslPreimageMultiAff, TConstraint )
import Isl.TypeLevel.Expr (TExpr)
import Isl.TypeLevel.Reflection
  ( DomTag(..)
  , KnownDom
  )
import Isl.TypeLevel.Sing (KnownExprs)


-- | The new VarDecl after tiling: same name, same value type,
-- new rank and ISL-computed domain.
type TiledVarDecl
       (ps :: [Symbol])
       (target :: Symbol)
       (factors :: [Maybe Nat])
       (oldDomCs :: [TConstraint ps (Length factors)])
       (a :: Type) =
  'VarDecl @ps
           @target
           @(Length factors + CountJust factors)
           @('Literal (IslPreimageMultiAff ps
                         (Length factors + CountJust factors)
                         (Length factors)
                         (TileMapExprs factors)
                         oldDomCs))
           @a


-- | Tile a variable by strip-mining selected dimensions.
--
-- @tile \"C\" '[Just 4, Nothing, Just 2] sys@ tiles dims 0 and 2
-- of variable @C@ by factors 4 and 2, leaving dim 1 untouched.
-- The new rank is @Length factors + CountJust factors@ (original
-- dims plus one tile index per tiled dim).
--
-- Delegates to 'reindex' with the strip-mining multi-aff
-- 'TileMapExprs'.  The new domain is computed by ISL via
-- 'IslPreimageMultiAff'.
tile
  :: forall (target :: Symbol) -> forall (factors :: [Maybe Nat]) ->
     forall ps inputs outputs locals
            (oldDomCs :: [TConstraint ps (Length factors)]) (a :: Type)
            (newDomCs :: [TConstraint ps (Length factors + CountJust factors)]).
     ( KnownSymbol target
     , KnownSymbols ps
     , KnownNat (Length factors)
     , KnownNat (Length factors + CountJust factors)
     , KnownNat (P.Length ps)
     , Lookup target (inputs ++ outputs ++ locals)
         ~ 'VarDecl @ps @target
                    @(Length factors)
                    @('Literal oldDomCs)
                    @a
     , newDomCs ~ IslPreimageMultiAff ps
                    (Length factors + CountJust factors)
                    (Length factors)
                    (TileMapExprs factors)
                    oldDomCs
     , KnownDom ps (Length factors + CountJust factors) ('Literal newDomCs)
     , KnownExprs ps (Length factors + CountJust factors) (TileMapExprs factors)
     )
  => System ps inputs outputs locals
  -> Either TransformError
            (System ps inputs
               (ReplaceDecl target (TiledVarDecl ps target factors oldDomCs a) outputs)
               (ReplaceDecl target (TiledVarDecl ps target factors oldDomCs a) locals))
tile (type target) (type factors) sys =
  reindex target (type (Length factors + CountJust factors)) (type (TileMapExprs factors)) sys
