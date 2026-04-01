{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Isl.HighLevel.MultiAff where

import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import GHC.TypeLits
import Control.Exception (evaluate)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicMap (BasicMap(..))
import Isl.HighLevel.Constraints
import Isl.HighLevel.Indices
import Isl.HighLevel.Map (Map(..))
import Isl.HighLevel.Set (Set(..))
import Isl.HighLevel.Params (KnownSymbols(..), Length)

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import qualified Isl.MultiAff as MA
import qualified Isl.BasicMap as BM
import qualified Isl.Map as RawMap
import qualified Isl.Set as RawSet
import qualified Isl.Aff as Aff
import qualified Isl.LocalSpace as LS
import qualified Isl.Space as Space
import qualified Isl.Val as Val

-- | Owned, parameter- and dimension-indexed MultiAff.
-- A functional map: ni input dims → no output dims, each output being
-- an affine expression of the inputs and parameters.
-- Linear — must be consumed exactly once.
newtype MultiAff (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = MultiAff Isl.MultiAff

-- | Borrowed reference to a MultiAff. Unrestricted — safe for queries.
newtype MultiAffRef (ps :: [Symbol]) (ni :: Nat) (no :: Nat) = MultiAffRef Isl.MultiAffRef

instance Consumable (MultiAff ps ni no) where
  consume = unsafeCoerce $ \(MultiAff ma) -> consume ma

instance Dupable (MultiAff ps ni no) where
  dup = unsafeCoerce $ \(MultiAff ma) ->
    let (a, b) = dup ma in (MultiAff a, MultiAff b)

instance Borrow (MultiAff ps ni no) (MultiAffRef ps ni no) where
  borrow = unsafeCoerce $ \(MultiAff ma) f ->
    let (result, ma') = borrow ma (\ref -> f (MultiAffRef ref))
    in (result, MultiAff ma')

-- Construction

-- | Build a MultiAff from a list of output expressions.
-- Each expression is over the input dimensions (as 'SetDim') and parameters (as 'SetParam').
-- The list length must equal @no@.
mkMultiAff
  :: forall ps ni no m. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => (IxList (Length ps) SetIx -> IxList ni SetIx -> [Expr SetIx])
  -> IslT m (MultiAff ps ni no)
mkMultiAff mkExprs = toMultiAff @ps @ni @no exprs
  where
    nParams = natVal (Proxy @(Length ps))
    paramList = coerceIxList $ mkIxListWith SetParam 0 nParams
    dimList = coerceIxList $ mkIxListWith SetDim 0 (natVal (Proxy @ni))
    exprs = mkExprs paramList dimList

-- | Build a MultiAff from a list of output expressions (value-level).
-- Expressions use 'SetDim' for input dims and 'SetParam' for parameters.
toMultiAff
  :: forall ps ni no m. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => [Expr SetIx] -> IslT m (MultiAff ps ni no)
toMultiAff exprs = do
  let nParams = fromIntegral $ natVal (Proxy @(Length ps))
      nIn     = fromIntegral $ natVal (Proxy @ni)
      nOut    = fromIntegral $ natVal (Proxy @no)
      paramNames = symbolVals @ps
  space0 <- Space.alloc nParams nIn nOut
  space <- foldM (\sp (i, name) -> Space.setDimName sp Isl.islDimParam i name)
                 space0
                 (zip [0..] paramNames)
  zma <- MA.zero space
  result <- foldM setOneAff zma (zip [0..] exprs)
  return (MultiAff result)
  where
    setOneAff ma (j, expr) = do
      let !(ref, ma') = borrow ma (\r -> r)
      domSp <- MA.getDomainSpace ref
      ls <- LS.fromSpace domSp
      aff <- exprToMultiAffAff ls expr
      MA.setAff ma' (fromIntegral j) aff

fromString :: forall m ps ni no. MonadIO m => String -> IslT m (MultiAff ps ni no)
fromString str = MultiAff <$> MA.readFromStr str

-- Operations (consuming)

-- | Compose two multi-affs: (nk → no) after (ni → nk) = (ni → no).
-- ISL's pullback convention: f.pullback(g) = f ∘ g.
pullback :: forall m ps ni nk no. MonadIO m
  => MultiAff ps nk no %1 -> MultiAff ps ni nk %1 -> IslT m (MultiAff ps ni no)
pullback = unsafeCoerce go
  where
    go :: MultiAff ps nk no -> MultiAff ps ni nk -> IslT m (MultiAff ps ni no)
    go (MultiAff f) (MultiAff g) = MultiAff <$> MA.pullbackMultiAff f g

-- | Convert to a relational map.
toMap :: forall m ps ni no. MonadIO m
  => MultiAff ps ni no %1 -> IslT m (Map ps ni no)
toMap = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> IslT m (Map ps ni no)
    go (MultiAff ma) = Map <$> RawMap.fromMultiAff ma

-- | Convert to a basic map (single conjunction of constraints).
toBasicMap :: forall m ps ni no. MonadIO m
  => MultiAff ps ni no %1 -> IslT m (BasicMap ps ni no)
toBasicMap = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> IslT m (BasicMap ps ni no)
    go (MultiAff ma) = BasicMap <$> BM.fromMultiAff ma

-- | Apply a multi-aff to a set (functional image).
apply :: forall m ps ni no. MonadIO m
  => MultiAff ps ni no %1 -> Set ps ni %1 -> IslT m (Set ps no)
apply = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> Set ps ni -> IslT m (Set ps no)
    go (MultiAff ma) (Set s) = do
      m <- RawMap.fromMultiAff ma
      Set <$> RawSet.apply s m

-- | Get the floor of each output expression.
floorMA :: forall m ps ni no. MonadIO m => MultiAff ps ni no %1 -> IslT m (MultiAff ps ni no)
floorMA = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> IslT m (MultiAff ps ni no)
    go (MultiAff ma) = MultiAff <$> MA.floor ma

-- Predicates (borrowing)

plainIsEqual :: forall m ps ni no. Monad m
  => MultiAff ps ni no %1 -> MultiAff ps ni no %1
  -> IslT m (Ur Bool, MultiAff ps ni no, MultiAff ps ni no)
plainIsEqual = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> MultiAff ps ni no
       -> IslT m (Ur Bool, MultiAff ps ni no, MultiAff ps ni no)
    go (MultiAff ma1) (MultiAff ma2) =
      let !(r, ma1') = borrow ma1 (\ref1 ->
            let !(r', ma2') = borrow ma2 (\ref2 -> MA.plainIsEqual ref1 ref2)
            in (r', ma2'))
          (innerR, ma2') = r
      in IslT $ \_ -> return (Ur innerR, MultiAff ma1', MultiAff ma2')

-- Queries

maToString :: MultiAffRef ps ni no -> String
maToString (MultiAffRef ref) = MA.toStr ref

borrowMA :: forall m ps ni no a. Monad m
  => MultiAff ps ni no %1 -> (MultiAffRef ps ni no -> a)
  -> IslT m (Ur a, MultiAff ps ni no)
borrowMA = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> (MultiAffRef ps ni no -> a)
       -> IslT m (Ur a, MultiAff ps ni no)
    go (MultiAff ma) f =
      let !(result, ma') = borrow ma (\ref -> f (MultiAffRef ref))
      in IslT $ \_ -> return (Ur result, MultiAff ma')

-- Decomposition

-- | Extract the list of output affine expressions as value-level 'Expr SetIx'.
-- Input dims are represented as 'SetDim', parameters as 'SetParam'.
decomposeMA :: forall m ps ni no. (MonadIO m, KnownNat ni, KnownNat no, KnownSymbols ps, KnownNat (Length ps))
  => MultiAff ps ni no %1 -> IslT m (Ur [Expr SetIx], MultiAff ps ni no)
decomposeMA = unsafeCoerce go
  where
    nParams = fromIntegral $ natVal (Proxy @(Length ps))
    nIn     = fromIntegral $ natVal (Proxy @ni)
    nOut    = fromIntegral $ natVal (Proxy @no)

    go :: MultiAff ps ni no -> IslT m (Ur [Expr SetIx], MultiAff ps ni no)
    go (MultiAff rawMA) = do
      let !(ref, rawMA') = borrow rawMA (\r -> r)
      exprs <- unsafeIslFromIO $ \_ ->
        forM [0 .. nOut - 1] $ \j -> do
          let maRef = let Isl.MultiAff ptr = rawMA' in Isl.MultiAffRef ptr
          aff <- MA.multiAffGetAffCopy maRef (fromIntegral j)
          let affR = let Isl.Aff ptr = aff in Isl.AffRef ptr
          expr <- extractAffExprIO nParams nIn affR
          evaluate (consume aff)
          return expr
      return (Ur exprs, MultiAff rawMA')

-- Resource management

freeMA :: forall m ps ni no. MonadIO m => MultiAff ps ni no %1 -> IslT m ()
freeMA = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> IslT m ()
    go (MultiAff ma) = freeM ma

consumingMA :: forall m ps ni no a. MonadIO m
  => MultiAff ps ni no %1 -> (MultiAffRef ps ni no -> a) -> IslT m (Ur a)
consumingMA = unsafeCoerce go
  where
    go :: MultiAff ps ni no -> (MultiAffRef ps ni no -> a) -> IslT m (Ur a)
    go (MultiAff ma) f = do
      let !(result, ma') = borrow ma (\ref -> f (MultiAffRef ref))
      freeM ma'
      return (Ur result)
