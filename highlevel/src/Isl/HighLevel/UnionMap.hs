{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isl.HighLevel.UnionMap where

import Control.Exception (evaluate)
import Control.Monad (forM, foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (catMaybes)
import Foreign.Ptr (nullPtr)
import GHC.TypeLits (someNatVal, SomeNat(..), KnownNat, Symbol)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.Constraints
  ( Conjunction(..), Constraint(..), MapIx(..)
  , extractMapDivs, extractMapConstraint, addMapConstraint )
import Isl.HighLevel.Map (Map(..))
import Isl.HighLevel.UnionSet (UnionSet(..))
import Isl.HighLevel.Pure

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..), SpaceRef(..), Ctx(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import qualified Isl.UnionMap as UM
import qualified Isl.UnionSet as USAutoGen
import qualified Isl.BasicMap as BM
import qualified Isl.Map as M
import qualified Isl.Space as Space

-- | Run an IslT IO action in raw IO context (safe when the action ignores ctx).
runIO :: IslT IO a -> IO a
runIO (IslT f) = f (Ctx nullPtr)

-- | Owned UnionMap. Not dimension-indexed because a union can contain
-- maps of different spaces.
newtype UnionMap = UnionMap Isl.UnionMap

-- | Borrowed reference to a UnionMap.
newtype UnionMapRef = UnionMapRef Isl.UnionMapRef

instance Consumable UnionMap where
  consume = unsafeCoerce $ \(UnionMap um) -> consume um

instance Dupable UnionMap where
  dup = unsafeCoerce $ \(UnionMap um) ->
    let (a, b) = dup um in (UnionMap a, UnionMap b)

instance Borrow UnionMap UnionMapRef where
  borrow = unsafeCoerce $ \(UnionMap um) f ->
    let (result, um') = borrow um (\ref -> f (UnionMapRef ref))
    in (result, UnionMap um')

-- Construction

fromMap :: forall m (ps :: [Symbol]) ni no. MonadIO m => Map ps ni no %1 -> IslT m UnionMap
fromMap = unsafeCoerce go
  where
    go :: Map ps ni no -> IslT m UnionMap
    go (Map m) = UnionMap <$> UM.fromMap m

fromString :: forall m. MonadIO m => String -> IslT m UnionMap
fromString str = UnionMap <$> UM.readFromStr str

-- Operations (consuming)

union :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
union = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.union um1 um2

intersect :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
intersect = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.intersect um1 um2

subtract :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
subtract = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.subtract um1 um2

coalesce :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
coalesce = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionMap
    go (UnionMap um) = UnionMap <$> UM.coalesce um

-- | Extract the domain of a union map as a union set. Consuming.
domain :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
domain = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionSet
    go (UnionMap um) = UnionSet <$> UM.domain um

-- | Extract the range of a union map as a union set. Consuming.
range :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionSet
range = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionSet
    go (UnionMap um) = UnionSet <$> UM.range um

-- | Reverse a union map (swap domain and range). Consuming.
reverse :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionMap
reverse = unsafeCoerce go
  where
    go :: UnionMap -> IslT m UnionMap
    go (UnionMap um) = UnionMap <$> UM.reverse um

-- | Compose two union maps on the domain side. Consuming.
applyDomain :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
applyDomain = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.applyDomain um1 um2

-- | Compose two union maps on the range side. Consuming.
applyRange :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
applyRange = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.applyRange um1 um2

-- | Flat range product of two union maps. Consuming.
flatRangeProduct :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
flatRangeProduct = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.flatRangeProduct um1 um2

-- | Lexicographic less-than relation between two union maps. Consuming.
lexLtUnionMap :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
lexLtUnionMap = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.lexLtUnionMap um1 um2

-- | Intersect the domain of a union map with a union set. Consuming.
intersectDomain :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
intersectDomain = unsafeCoerce go
  where
    go :: UnionMap -> UnionSet -> IslT m UnionMap
    go (UnionMap um) (UnionSet us) = UnionMap <$> UM.intersectDomain um us

-- | Simplify a union map given a context (another union map). Consuming.
gist :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m UnionMap
gist = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m UnionMap
    go (UnionMap um1) (UnionMap um2) = UnionMap <$> UM.gist um1 um2

-- | Simplify a union map given a domain context (a union set). Consuming.
gistDomain :: forall m. MonadIO m => UnionMap %1 -> UnionSet %1 -> IslT m UnionMap
gistDomain = unsafeCoerce go
  where
    go :: UnionMap -> UnionSet -> IslT m UnionMap
    go (UnionMap um) (UnionSet us) = UnionMap <$> UM.gistDomain um us

-- Lift pure representations to ISL objects

-- | Construct an ISL 'UnionMap' from a 'NamedMap' (pure representation).
-- Sets the domain tuple name and parameter names on the ISL space.
toUnionMapFromNamed :: forall m. MonadIO m => NamedMap -> IslT m UnionMap
toUnionMapFromNamed nm = do
  let nParams = length (nmParams nm)
      nIn     = nmNIn nm
      nOut    = nmNOut nm
  maps <- mapM (buildOneMap nParams nIn nOut (nmParams nm) (nmDomainName nm)) (nmConjs nm)
  case maps of
    []     -> do
      sp <- Space.alloc (fromIntegral nParams) (fromIntegral nIn) (fromIntegral nOut)
      sp' <- setParamNames sp (nmParams nm)
      sp'' <- case nmDomainName nm of
                Just name -> Space.setTupleName sp' Isl.islDimIn name
                Nothing   -> return sp'
      UnionMap <$> UM.emptySpace sp''
    (m:ms) -> do
      um0 <- UnionMap <$> UM.fromMap m
      foldM (\(UnionMap acc) m' -> do
                um' <- UM.fromMap m'
                UnionMap <$> UM.union acc um')
            um0 ms
  where
    buildOneMap nP nI nO paramNames domName (Conjunction constraints) = do
      space0 <- Space.alloc (fromIntegral nP) (fromIntegral nI) (fromIntegral nO)
      space1 <- setParamNames space0 paramNames
      space2 <- case domName of
                  Just name -> Space.setTupleName space1 Isl.islDimIn name
                  Nothing   -> return space1
      univ <- BM.universe space2
      bm <- foldM addMapConstraint univ constraints
      M.fromBasicMap bm

    setParamNames sp names =
      foldM (\s (i, name) -> Space.setDimName s Isl.islDimParam i name)
            sp (zip [0..] names)

-- | Apply a union map to a union set. This is the core operation for
-- computing scheduled domains: @applyToSet domain schedule@ gives the
-- image of @domain@ under @schedule@.
--
-- Both arguments are consumed (__isl_take).
applyToSet :: forall m. MonadIO m => UnionSet %1 -> UnionMap %1 -> IslT m UnionSet
applyToSet = unsafeCoerce go
  where
    go :: UnionSet -> UnionMap -> IslT m UnionSet
    go (UnionSet us) (UnionMap um) = UnionSet <$> USAutoGen.apply us um

-- Predicates (borrowing)

isEmpty :: forall m. Monad m => UnionMap %1 -> IslT m (Ur Bool, UnionMap)
isEmpty = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur Bool, UnionMap)
    go (UnionMap um) =
      let !(ref, um') = borrow um (\r -> r)
          !r = UM.isEmpty ref
      in IslT $ \_ -> return (Ur r, UnionMap um')

isEqual :: forall m. Monad m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool, UnionMap, UnionMap)
isEqual = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool, UnionMap, UnionMap)
    go (UnionMap um1) (UnionMap um2) =
      let !(ref1, um1') = borrow um1 (\r -> r)
          !(ref2, um2') = borrow um2 (\r -> r)
          !r = UM.isEqual ref1 ref2
      in IslT $ \_ -> return (Ur r, UnionMap um1', UnionMap um2')

isSubset :: forall m. Monad m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool, UnionMap, UnionMap)
isSubset = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool, UnionMap, UnionMap)
    go (UnionMap um1) (UnionMap um2) =
      let !(ref1, um1') = borrow um1 (\r -> r)
          !(ref2, um2') = borrow um2 (\r -> r)
          !r = UM.isSubset ref1 ref2
      in IslT $ \_ -> return (Ur r, UnionMap um1', UnionMap um2')

-- Queries

umapToString :: UnionMapRef -> String
umapToString (UnionMapRef umRef) = UM.toStr umRef

borrowUM :: forall m a. Monad m => UnionMap %1 -> (UnionMapRef -> a) -> IslT m (Ur a, UnionMap)
borrowUM = unsafeCoerce go
  where
    go :: UnionMap -> (UnionMapRef -> a) -> IslT m (Ur a, UnionMap)
    go (UnionMap um) f =
      let !(result, um') = borrow um (\ref -> f (UnionMapRef ref))
      in IslT $ \_ -> return (Ur result, UnionMap um')

-- Decomposition

decomposeUnionMap :: forall m. MonadIO m
  => UnionMap %1 -> IslT m (Ur [SomeMapDisjunction], UnionMap)
decomposeUnionMap = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur [SomeMapDisjunction], UnionMap)
    go (UnionMap rawUm) = do
      let !(ref, rawUm') = borrow rawUm (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        UM.foreachMap ref $ \m -> do
          let !(mRef, _) = borrow m (\r -> r)
          space <- runIO $ M.getSpace mRef
          let spRef = SpaceRef (Isl.unSpace space)
              nIn  = Space.dim spRef Isl.islDimIn
              nOut = Space.dim spRef Isl.islDimOut
              nParams = Space.dim spRef Isl.islDimParam
          evaluate (consume space)
          conjunctions <- M.foreachBasicMap mRef $ \bm -> do
            let !(bmRef, _) = borrow bm (\r -> r)
                !rawBmRef = Isl.BasicMapRef (Isl.unBasicMap bm)
            divExprs <- extractMapDivs rawBmRef nIn nOut nParams
            constraints <- BM.foreachConstraint bmRef $ \c -> do
              result <- extractMapConstraint nParams nIn nOut divExprs c
              evaluate (consume c)
              return result
            evaluate (consume bm)
            return (Conjunction constraints)
          evaluate (consume m)
          return (nIn, nOut, conjunctions)
      return (Ur (map wrapMapDisjunction results), UnionMap rawUm')

wrapMapDisjunction :: (Int, Int, [Conjunction MapIx]) -> SomeMapDisjunction
wrapMapDisjunction (nIn, nOut, conjs) =
  case (someNatVal (fromIntegral nIn), someNatVal (fromIntegral nOut)) of
    (Just (SomeNat (_ :: proxy ni)), Just (SomeNat (_ :: proxy no))) ->
      SomeMapDisjunction (unsafeCoerce (PMapDisjunction (map PMapConjunction conjs)) :: PMapDisjunction '[] ni no)
    _ -> error "wrapMapDisjunction: negative dimension count"

-- Named decomposition

-- | Decompose a UnionMap into per-space 'NamedMap's, preserving domain tuple
-- names and parameter names. Used for extracting per-statement schedule inverses.
--
-- Borrows the UnionMap (returned alongside the result).
decomposeUnionMapNamed :: forall m. MonadIO m
  => UnionMap %1 -> IslT m (Ur [NamedMap], UnionMap)
decomposeUnionMapNamed = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur [NamedMap], UnionMap)
    go (UnionMap rawUm) = do
      let !(ref, rawUm') = borrow rawUm (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        UM.foreachMap ref $ \m -> do
          let !(mRef, _) = borrow m (\r -> r)
          space <- runIO $ M.getSpace mRef
          let spRef = SpaceRef (Isl.unSpace space)
              nIn  = Space.dim spRef Isl.islDimIn
              nOut = Space.dim spRef Isl.islDimOut
              nParams = Space.dim spRef Isl.islDimParam
          domainName <- Space.spaceGetTupleName space Isl.islDimIn
          rangeName  <- Space.spaceGetTupleName space Isl.islDimOut
          paramNames <- forM [0 .. nParams - 1] $ \i ->
            Space.spaceGetDimName space Isl.islDimParam i
          evaluate (consume space)
          conjunctions <- M.foreachBasicMap mRef $ \bm -> do
            let !(bmRef, _) = borrow bm (\r -> r)
                !rawBmRef = Isl.BasicMapRef (Isl.unBasicMap bm)
            divExprs <- extractMapDivs rawBmRef nIn nOut nParams
            constraints <- BM.foreachConstraint bmRef $ \c -> do
              result <- extractMapConstraint nParams nIn nOut divExprs c
              evaluate (consume c)
              return result
            evaluate (consume bm)
            return (Conjunction constraints)
          evaluate (consume m)
          return NamedMap
            { nmDomainName = domainName
            , nmRangeName  = rangeName
            , nmParams     = catMaybes paramNames
            , nmNIn        = nIn
            , nmNOut       = nOut
            , nmConjs      = conjunctions
            }
      return (Ur results, UnionMap rawUm')

-- Resource management

freeUnionMap :: forall m. MonadIO m => UnionMap %1 -> IslT m ()
freeUnionMap = unsafeCoerce go
  where
    go :: UnionMap -> IslT m ()
    go (UnionMap um) = freeM um

-- Consuming combinators

-- | Borrow a UnionMap for a query, then free it.
consumingUM :: forall m a. MonadIO m => UnionMap %1 -> (UnionMapRef -> a) -> IslT m (Ur a)
consumingUM = unsafeCoerce go
  where
    go :: UnionMap -> (UnionMapRef -> a) -> IslT m (Ur a)
    go (UnionMap um) f = do
      let !(result, um') = borrow um (\ref -> f (UnionMapRef ref))
      freeM um'
      return (Ur result)

-- | Check if a UnionMap is empty, then free it.
consumingIsEmpty :: forall m. MonadIO m => UnionMap %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go :: UnionMap -> IslT m (Ur Bool)
    go (UnionMap um) = do
      let !(ref, um') = borrow um (\r -> r)
          !r = UM.isEmpty ref
      freeM um'
      return (Ur r)

-- | Check equality of two UnionMaps, then free both.
consumingIsEqual :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool)
consumingIsEqual = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool)
    go (UnionMap um1) (UnionMap um2) = do
      let !(ref1, um1') = borrow um1 (\r -> r)
          !(ref2, um2') = borrow um2 (\r -> r)
          !r = UM.isEqual ref1 ref2
      freeM um1'; freeM um2'
      return (Ur r)

-- | Check if first UnionMap is a subset of the second, then free both.
consumingIsSubset :: forall m. MonadIO m => UnionMap %1 -> UnionMap %1 -> IslT m (Ur Bool)
consumingIsSubset = unsafeCoerce go
  where
    go :: UnionMap -> UnionMap -> IslT m (Ur Bool)
    go (UnionMap um1) (UnionMap um2) = do
      let !(ref1, um1') = borrow um1 (\r -> r)
          !(ref2, um2') = borrow um2 (\r -> r)
          !r = UM.isSubset ref1 ref2
      freeM um1'; freeM um2'
      return (Ur r)
