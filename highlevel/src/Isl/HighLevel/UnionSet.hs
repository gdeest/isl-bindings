{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Isl.HighLevel.UnionSet where

import Control.Exception (evaluate)
import Control.Monad (forM, foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (catMaybes)
import Foreign.Ptr (nullPtr)
import GHC.TypeLits (someNatVal, SomeNat(..), KnownNat, Symbol)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.HighLevel.BasicSet (extractSetConstraint)
import Isl.HighLevel.Constraints
  ( Conjunction(..), Constraint(..), SetIx(..), expandExpr )
import Isl.HighLevel.Pure
import Isl.HighLevel.Set (Set(..))

import qualified Isl.Types as Isl
import Isl.Types (Borrow(..), Dupable(..), Consumable(..))
import Isl.Monad (IslT(..), Ur(..), unsafeIslFromIO, freeM)
import qualified Isl.UnionSet as US
import qualified Isl.BasicSet as BS
import qualified Isl.Set as S
import qualified Isl.Constraint as Constraint
import qualified Isl.LocalSpace as LS
import qualified Isl.Space as Space

-- | Helper to run an IslT IO action from plain IO.
-- Used in foreach callbacks where we need to call MonadicGive functions.
-- Safe because those functions ignore the Ctx argument.
runIO :: IslT IO a -> IO a
runIO (IslT f) = f (Isl.Ctx nullPtr)

-- | Owned UnionSet. Not dimension-indexed because a union can contain
-- sets of different spaces.
newtype UnionSet = UnionSet Isl.UnionSet

-- | Borrowed reference to a UnionSet.
newtype UnionSetRef = UnionSetRef Isl.UnionSetRef

instance Consumable UnionSet where
  consume = unsafeCoerce $ \(UnionSet us) -> consume us

instance Dupable UnionSet where
  dup = unsafeCoerce $ \(UnionSet us) ->
    let (a, b) = dup us in (UnionSet a, UnionSet b)

instance Borrow UnionSet UnionSetRef where
  borrow = unsafeCoerce $ \(UnionSet us) f ->
    let (result, us') = borrow us (\ref -> f (UnionSetRef ref))
    in (result, UnionSet us')

-- Construction

fromSet :: forall m (ps :: [Symbol]) n. MonadIO m => Set ps n %1 -> IslT m UnionSet
fromSet = unsafeCoerce go
  where
    go :: Set ps n -> IslT m UnionSet
    go (Set s) = UnionSet <$> US.fromSet s

fromString :: forall m. MonadIO m => String -> IslT m UnionSet
fromString str = UnionSet <$> US.readFromStr str

-- Operations (consuming)

union :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
union = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m UnionSet
    go (UnionSet us1) (UnionSet us2) = UnionSet <$> US.union us1 us2

intersect :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
intersect = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m UnionSet
    go (UnionSet us1) (UnionSet us2) = UnionSet <$> US.intersect us1 us2

subtract :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
subtract = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m UnionSet
    go (UnionSet us1) (UnionSet us2) = UnionSet <$> US.subtract us1 us2

coalesce :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
coalesce = unsafeCoerce go
  where
    go :: UnionSet -> IslT m UnionSet
    go (UnionSet us) = UnionSet <$> US.coalesce us

-- Predicates (borrowing)

isEmpty :: forall m. Monad m => UnionSet %1 -> IslT m (Ur Bool, UnionSet)
isEmpty = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur Bool, UnionSet)
    go (UnionSet us) =
      let !(r, us') = borrow us (\ref -> US.isEmpty ref)
      in IslT $ \_ -> return (Ur r, UnionSet us')

isEqual :: forall m. Monad m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool, UnionSet, UnionSet)
isEqual = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool, UnionSet, UnionSet)
    go (UnionSet us1) (UnionSet us2) =
      let !(ref1, us1') = borrow us1 (\r -> r)
          !(ref2, us2') = borrow us2 (\r -> r)
          !r = US.isEqual ref1 ref2
      in IslT $ \_ -> return (Ur r, UnionSet us1', UnionSet us2')

isSubset :: forall m. Monad m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool, UnionSet, UnionSet)
isSubset = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool, UnionSet, UnionSet)
    go (UnionSet us1) (UnionSet us2) =
      let !(ref1, us1') = borrow us1 (\r -> r)
          !(ref2, us2') = borrow us2 (\r -> r)
          !r = US.isSubset ref1 ref2
      in IslT $ \_ -> return (Ur r, UnionSet us1', UnionSet us2')

-- Queries

usetToString :: UnionSetRef -> String
usetToString (UnionSetRef usRef) = US.toStr usRef

borrowUS :: forall m a. Monad m => UnionSet %1 -> (UnionSetRef -> a) -> IslT m (Ur a, UnionSet)
borrowUS = unsafeCoerce go
  where
    go :: UnionSet -> (UnionSetRef -> a) -> IslT m (Ur a, UnionSet)
    go (UnionSet us) f =
      let !(result, us') = borrow us (\ref -> f (UnionSetRef ref))
      in IslT $ \_ -> return (Ur result, UnionSet us')

-- Decomposition
--
-- Note: Parameters are extracted at runtime and wrapped existentially
-- since the parameter names are not known statically for union types.
-- For now, we extract with 0 params (matching old behavior).
-- TODO: read parameter names from ISL space and wrap with KnownSymbols.

decomposeUnionSet :: forall m. MonadIO m
  => UnionSet %1 -> IslT m (Ur [SomeDisjunction], UnionSet)
decomposeUnionSet = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur [SomeDisjunction], UnionSet)
    go (UnionSet rawUs) = do
      let !(ref, rawUs') = borrow rawUs (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        US.foreachSet ref $ \s -> do
          let !(sRef, _) = borrow s (\r -> r)
          space <- runIO $ S.getSpace sRef
          let spRef = Isl.SpaceRef (Isl.unSpace space)
              nDims = Space.dim spRef Isl.islDimSet
              nParams = Space.dim spRef Isl.islDimParam
          evaluate (consume space)
          conjunctions <- S.foreachBasicSet sRef $ \bs -> do
            let !(bsRef, _) = borrow bs (\r -> r)
            constraints <- BS.foreachConstraint bsRef $ \c -> do
              result <- extractSetConstraint nParams nDims c
              evaluate (consume c)
              return result
            evaluate (consume bs)
            return (Conjunction constraints)
          evaluate (consume s)
          return (nDims, conjunctions)
      return (Ur (map wrapDisjunction results), UnionSet rawUs')

wrapDisjunction :: (Int, [Conjunction SetIx]) -> SomeDisjunction
wrapDisjunction (nDims, conjs) =
  case someNatVal (fromIntegral nDims) of
    Just (SomeNat (_ :: proxy n)) ->
      -- Wrap with empty params for now; full param recovery is a TODO
      SomeDisjunction (unsafeCoerce (PDisjunction (map PConjunction conjs)) :: PDisjunction '[] n)
    Nothing -> error "wrapDisjunction: negative dimension count"

-- Lift pure representations to ISL objects

-- | Construct an ISL 'UnionSet' from a 'NamedSet' (pure representation).
-- Sets the tuple name and parameter names on the ISL space, builds
-- a BasicSet per conjunction, promotes to Set, unions them into a UnionSet.
toUnionSetFromNamed :: forall m. MonadIO m => NamedSet -> IslT m UnionSet
toUnionSetFromNamed ns = do
  let nParams = length (nsParams ns)
      nDims   = nsNDims ns
  sets <- mapM (buildOneSet nParams nDims (nsParams ns) (nsName ns)) (nsConjs ns)
  case sets of
    []     -> do
      -- Empty: build an empty union set
      sp <- Space.setAlloc (fromIntegral nParams) (fromIntegral nDims)
      sp' <- setParamNames sp (nsParams ns)
      sp'' <- case nsName ns of
                Just name -> Space.setTupleName sp' Isl.islDimSet name
                Nothing   -> return sp'
      UnionSet <$> US.emptySpace sp''
    (s:ss) -> do
      us0 <- UnionSet <$> US.fromSet s
      foldM (\(UnionSet acc) s' -> do
                us' <- US.fromSet s'
                UnionSet <$> US.union acc us')
            us0 ss
  where
    buildOneSet nP nD paramNames tupleName (Conjunction constraints) = do
      space0 <- Space.setAlloc (fromIntegral nP) (fromIntegral nD)
      space1 <- setParamNames space0 paramNames
      space2 <- case tupleName of
                  Just name -> Space.setTupleName space1 Isl.islDimSet name
                  Nothing   -> return space1
      univ <- BS.universe space2
      bs <- foldM addSetConstraint univ constraints
      S.fromBasicSet bs

    setParamNames sp names =
      foldM (\s (i, name) -> Space.setDimName s Isl.islDimParam i name)
            sp (zip [0..] names)

    addSetConstraint bs constraint = do
      let !(ref, bs') = borrow bs (\r -> r)
      sp <- BS.getSpace ref
      ls <- LS.fromSpace sp
      (emptyC, e) <- case constraint of
        InequalityConstraint e -> do
          co <- Constraint.inequalityAlloc ls
          return (co, e)
        EqualityConstraint e -> do
          co <- Constraint.equalityAlloc ls
          return (co, e)
      let (coeffs, constant) = expandExpr e
          setCoeff constr (coeff, ix) = do
            let (dimType, pos) = case ix of
                  SetDim i  -> (Isl.islDimSet, i)
                  SetParam i -> (Isl.islDimParam, i)
            Constraint.setCoefficientSi constr dimType (fromIntegral pos) (fromIntegral coeff)
      linearPart <- foldM setCoeff emptyC coeffs
      finalC <- Constraint.setConstantSi linearPart (fromIntegral constant)
      BS.addConstraint bs' finalC

-- Named decomposition

-- | Decompose a UnionSet into per-space 'NamedSet's, preserving tuple names
-- and parameter names from the ISL spaces. This is the primary interface for
-- multi-statement programs where tuple names identify statements.
--
-- Borrows the UnionSet (returned alongside the result).
decomposeUnionSetNamed :: forall m. MonadIO m
  => UnionSet %1 -> IslT m (Ur [NamedSet], UnionSet)
decomposeUnionSetNamed = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur [NamedSet], UnionSet)
    go (UnionSet rawUs) = do
      let !(ref, rawUs') = borrow rawUs (\r -> r)
      results <- unsafeIslFromIO $ \_ ->
        US.foreachSet ref $ \s -> do
          let !(sRef, _) = borrow s (\r -> r)
          space <- runIO $ S.getSpace sRef
          let spRef = Isl.SpaceRef (Isl.unSpace space)
              nDims = Space.dim spRef Isl.islDimSet
              nParams = Space.dim spRef Isl.islDimParam
          tupleName <- Space.spaceGetTupleName space Isl.islDimSet
          paramNames <- forM [0 .. nParams - 1] $ \i ->
            Space.spaceGetDimName space Isl.islDimParam i
          evaluate (consume space)
          conjunctions <- S.foreachBasicSet sRef $ \bs -> do
            let !(bsRef, _) = borrow bs (\r -> r)
            constraints <- BS.foreachConstraint bsRef $ \c -> do
              result <- extractSetConstraint nParams nDims c
              evaluate (consume c)
              return result
            evaluate (consume bs)
            return (Conjunction constraints)
          evaluate (consume s)
          return NamedSet
            { nsName   = tupleName
            , nsParams = catMaybes paramNames
            , nsNDims  = nDims
            , nsConjs  = conjunctions
            }
      return (Ur results, UnionSet rawUs')

-- Resource management

freeUnionSet :: forall m. MonadIO m => UnionSet %1 -> IslT m ()
freeUnionSet = unsafeCoerce go
  where
    go :: UnionSet -> IslT m ()
    go (UnionSet us) = freeM us

-- Consuming combinators

-- | Borrow a UnionSet for a query, then free it.
consumingUS :: forall m a. MonadIO m => UnionSet %1 -> (UnionSetRef -> a) -> IslT m (Ur a)
consumingUS = unsafeCoerce go
  where
    go :: UnionSet -> (UnionSetRef -> a) -> IslT m (Ur a)
    go (UnionSet us) f = do
      let !(result, us') = borrow us (\ref -> f (UnionSetRef ref))
      freeM us'
      return (Ur result)

-- | Check if a UnionSet is empty, then free it.
consumingIsEmpty :: forall m. MonadIO m => UnionSet %1 -> IslT m (Ur Bool)
consumingIsEmpty = unsafeCoerce go
  where
    go :: UnionSet -> IslT m (Ur Bool)
    go (UnionSet us) = do
      let !(r, us') = borrow us (\ref -> US.isEmpty ref)
      freeM us'
      return (Ur r)

-- | Check equality of two UnionSets, then free both.
consumingIsEqual :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool)
consumingIsEqual = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool)
    go (UnionSet us1) (UnionSet us2) = do
      let !(ref1, us1') = borrow us1 (\r -> r)
          !(ref2, us2') = borrow us2 (\r -> r)
          !r = US.isEqual ref1 ref2
      freeM us1'; freeM us2'
      return (Ur r)

-- | Check if first UnionSet is a subset of the second, then free both.
consumingIsSubset :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m (Ur Bool)
consumingIsSubset = unsafeCoerce go
  where
    go :: UnionSet -> UnionSet -> IslT m (Ur Bool)
    go (UnionSet us1) (UnionSet us2) = do
      let !(ref1, us1') = borrow us1 (\r -> r)
          !(ref2, us2') = borrow us2 (\r -> r)
          !r = US.isSubset ref1 ref2
      freeM us1'; freeM us2'
      return (Ur r)
