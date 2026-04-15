{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}

-- | Haskell representation of affine constraints and expressions,
-- plus bridge functions to construct/decompose ISL objects.
module Isl.Typed.Constraints
  ( -- * Index types
    SetIx(..)
  , MapIx(..)
  , MapDim
    -- * Expressions
  , Expr(..)
  , modExpr
  , cst, idx
  , (+:), (-:), (*:)
    -- * Constraints
  , Constraint(..)
  , (<=:), (>=:), (==:)
    -- * Conjunctions
  , Conjunction(..)
  , (&&:)
  , ToConjunction(..)
    -- * Expression utilities
  , rebuildExpr
  , rebuildExprWithDivs
  , expandExpr
  , hasFloorDiv
  , conjunctionHasFloorDiv
    -- * ISL extraction (ISL object -> Haskell)
  , extractAffExprIO
  , extractSetAffExprIO
  , extractSetDivs
  , extractMapDivs
  , extractSetConstraint
  , extractMapConstraint
    -- * ISL construction (Haskell -> ISL object)
  , exprToAff
  , exprToSetAff
  , exprToMapAff
  , constraintFromSetExpr
  , constraintFromMapExpr
  , addSetConstraint
  , addMapConstraint
    -- * High-level construction (Conjunction -> ISL object)
  , buildBasicSet
  , buildBasicMap
  , buildMultiAff
    -- * High-level decomposition (ISL object -> pure representation)
  , decomposeBasicSet
  , decomposeBasicMap
  , decomposeSet
  , decomposeMap
  , decomposeMultiAff
    -- * Named (value-level) representations
  , NamedSet(..)
  , NamedMap(..)
    -- * Union map bridge (NamedMap <-> ISL)
  , buildUnionMapFromNamed
  , decomposeUnionMapNamed
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Foreign.Ptr (nullPtr)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import qualified Isl.Types as Isl
import Isl.Types (Ctx(..))
import Isl.Types.Internal (Consumable(..), Borrow(..))
import Isl.Monad.Internal (IslT(..), Ur(..), unsafeIslFromIO)
import Isl.Linear (freeM)
import qualified Isl.Linear as Isl
import qualified Isl.Aff as Aff
import qualified Isl.BasicSet as BS
import qualified Isl.BasicMap as BM
import qualified Isl.Constraint as Constraint
import qualified Isl.LocalSpace as LS
import qualified Isl.Map as M
import qualified Isl.MultiAff as MA
import qualified Isl.Set as S
import qualified Isl.Space as Space
import qualified Isl.UnionMap as UM
import qualified Isl.Val as Val

-- =========================================================================
-- Index types
-- =========================================================================

-- | Dimension index for set constraints, distinguishing dimensions from parameters.
data SetIx = SetDim !Int | SetParam !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData SetIx

-- | Dimension index for map constraints, distinguishing input, output, and parameters.
data MapIx = InDim !Int | OutDim !Int | MapParam !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData MapIx

-- | Legacy alias.
type MapDim = MapIx

-- =========================================================================
-- Expressions
-- =========================================================================

-- | Affine expressions (with floor division for existentials).
data Expr ix
  = Ix ix                        -- ^ Variable reference
  | Constant Integer             -- ^ Integer constant
  | Mul Integer (Expr ix)        -- ^ Scalar multiplication
  | Add (Expr ix) (Expr ix)      -- ^ Addition
  | FloorDiv (Expr ix) Integer   -- ^ @floor(expr / d)@ — from ISL existentials
  deriving (Generic, Functor)

-- | @modExpr a b@ = @a mod b@, expressed as @a - b * floor(a / b)@.
modExpr :: Expr ix -> Integer -> Expr ix
modExpr a b = Add a (Mul (-b) (FloorDiv a b))

instance NFData ix => NFData (Expr ix)

deriving instance Show ix => Show (Expr ix)
deriving instance Eq ix => Eq (Expr ix)

infixl 4 -:
infixl 4 +:
infixr 5 *:

infix 3 ==:
infix 3 <=:
infix 3 >=:

infixl 2 &&:

(-:) :: Expr ix -> Expr ix -> Expr ix
e1 -: e2 = e1 +: (-1) *: e2

(+:) :: Expr ix -> Expr ix -> Expr ix
e1 +: e2 = Add e1 e2

(*:) :: Integer -> Expr ix -> Expr ix
k *: e1 = Mul k e1

cst :: Integer -> Expr ix
cst = Constant

idx :: ix -> Expr ix
idx = Ix

-- =========================================================================
-- Constraints
-- =========================================================================

-- | Affine equality / inequality constraints.
data Constraint ix
  = EqualityConstraint (Expr ix)
    -- ^ @EqualityConstraint e@ represents @e = 0@.
  | InequalityConstraint (Expr ix)
    -- ^ @InequalityConstraint e@ represents @e >= 0@.
  deriving (Generic)

instance NFData ix => NFData (Constraint ix)

deriving instance Show ix => Show (Constraint ix)
deriving instance Eq ix => Eq (Constraint ix)

(<=:) :: Expr ix -> Expr ix -> Constraint ix
e1 <=: e2 = InequalityConstraint $ Add e2 (Mul (-1) e1)

(>=:) :: Expr ix -> Expr ix -> Constraint ix
e1 >=: e2 = e2 <=: e1

(==:) :: Expr ix -> Expr ix -> Constraint ix
e1 ==: e2 = EqualityConstraint $ Add e2 (Mul (-1) e1)

-- =========================================================================
-- Conjunctions
-- =========================================================================

-- | A conjunction of constraints, defining a single convex polyhedron.
newtype Conjunction ix = Conjunction [Constraint ix]
  deriving (Generic)

instance NFData ix => NFData (Conjunction ix)

deriving instance Show ix => Show (Conjunction ix)
deriving instance Eq ix => Eq (Conjunction ix)

(&&:) :: ToConjunction c => c ix -> Constraint ix -> Conjunction ix
(&&:) conj constr = Conjunction (constr:cs)
  where Conjunction cs = toConjunction conj

class ToConjunction (c :: Type -> Type) where
  toConjunction :: c ix -> Conjunction ix

instance ToConjunction Conjunction where
  toConjunction = id

instance ToConjunction Constraint where
  toConjunction = Conjunction . pure

-- =========================================================================
-- Expression utilities
-- =========================================================================

rebuildExpr :: [(Integer, ix)] -> Integer -> Expr ix
rebuildExpr coeffs constant =
  let terms = [if c == 1 then Ix i else Mul c (Ix i) | (c, i) <- coeffs, c /= 0]
      constTerm = [Constant constant | constant /= 0]
      allTerms = terms ++ constTerm
  in case allTerms of
    []     -> Constant 0
    [t]    -> t
    (t:ts) -> foldl Add t ts

rebuildExprWithDivs :: [(Integer, Expr ix)] -> Integer -> Expr ix
rebuildExprWithDivs coeffs constant =
  let terms = [if c == 1 then e else if c == -1 then Mul (-1) e else Mul c e
              | (c, e) <- coeffs]
      constTerm = [Constant constant | constant /= 0]
      allTerms = terms ++ constTerm
  in case allTerms of
    []     -> Constant 0
    [t]    -> t
    (t:ts) -> foldl Add t ts

expandExpr :: Ord ix => Expr ix -> ([(Integer, ix)], Integer)
expandExpr (Ix ix) = ([(1, ix)], 0)
expandExpr (Constant k) = ([], k)
expandExpr (Mul k e) =
  ((\(k', ix) -> (k*k', ix)) <$> linearTerms, k*co)
  where (linearTerms, co) = expandExpr e
expandExpr (Add e1 e2) = merge (expandExpr e1) (expandExpr e2)
  where merge (lt1, c1) (lt2, c2) = (mergeTerms lt1 lt2, c1+c2)
        mergeTerms [] terms = terms
        mergeTerms terms [] = terms
        mergeTerms ts1@(t1@(coeff1, ix1):rst1) ts2@(t2@(coeff2, ix2):rst2)
          | ix1 < ix2 = t1:(mergeTerms rst1 ts2)
          | ix2 < ix1 = t2:(mergeTerms ts1 rst2)
          | otherwise = (coeff1+coeff2, ix1):(mergeTerms rst1 rst2)
expandExpr (FloorDiv _ _) = error "expandExpr: FloorDiv cannot be linearized — use exprToSetAff/exprToMapAff"

hasFloorDiv :: Expr ix -> Bool
hasFloorDiv (Ix _)          = False
hasFloorDiv (Constant _)    = False
hasFloorDiv (Mul _ e)       = hasFloorDiv e
hasFloorDiv (Add a b)       = hasFloorDiv a || hasFloorDiv b
hasFloorDiv (FloorDiv _ _)  = True

conjunctionHasFloorDiv :: [Constraint ix] -> Bool
conjunctionHasFloorDiv = any go
  where
    go (EqualityConstraint e)   = hasFloorDiv e
    go (InequalityConstraint e) = hasFloorDiv e

-- =========================================================================
-- ISL extraction (ISL object -> Haskell)
-- =========================================================================

extractAffExprIO :: Int -> Int -> Isl.AffRef -> IO (Expr SetIx)
extractAffExprIO nParams nIn affR = do
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimParam (fromIntegral i)
    return (c, SetParam i)
  inCoeffs <- forM [0 .. nIn - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimIn (fromIntegral i)
    return (c, SetDim i)
  constant <- Aff.affGetConstantSi affR
  let allCoeffs = filter (\(c, _) -> c /= 0) (paramCoeffs ++ inCoeffs)
  return $ rebuildExpr allCoeffs constant

extractSetAffExprIO :: Int -> Int -> Isl.AffRef -> IO (Expr SetIx)
extractSetAffExprIO nParams nDims affR = do
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimParam (fromIntegral i)
    return (c, SetParam i)
  dimCoeffs <- forM [0 .. nDims - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimIn (fromIntegral i)
    return (c, SetDim i)
  constant <- Aff.affGetConstantSi affR
  let allCoeffs = filter (\(c, _) -> c /= 0) (paramCoeffs ++ dimCoeffs)
  return $ rebuildExpr allCoeffs constant

-- =========================================================================
-- Div-aware extraction
-- =========================================================================

extractSetDivs :: Isl.BasicSetRef -> Int -> Int -> IO [Expr SetIx]
extractSetDivs bsRef nDims nParams = do
  let nDivs = BS.dim bsRef Isl.islDimDiv
  go 0 nDivs []
  where
    go i nDivs acc
      | i >= nDivs = return (reverse acc)
      | otherwise = do
          expr <- extractOneSetDiv bsRef nDims nParams (reverse acc) i
          go (i + 1) nDivs (expr : acc)

extractOneSetDiv :: Isl.BasicSetRef -> Int -> Int -> [Expr SetIx] -> Int -> IO (Expr SetIx)
extractOneSetDiv bsRef nDims nParams earlierDivs divIdx = do
  aff <- BS.c_getDiv bsRef (fromIntegral divIdx)
  let !affR = Isl.AffRef (Isl.unAff aff)
  denomVal <- Aff.c_getDenominatorVal affR
  let !denom = fromIntegral $ Val.getNumSi (Isl.ValRef (Isl.unVal denomVal))
  evaluate (consume denomVal)
  dimCoeffs <- forM [0 .. nDims - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimIn (fromIntegral i)
    return (c, Ix (SetDim i))
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimParam (fromIntegral i)
    return (c, Ix (SetParam i))
  divCoeffs <- forM (zip [0..] earlierDivs) $ \(i, divExpr) -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimDiv (fromIntegral (i :: Int))
    return (c, divExpr)
  constVal <- Aff.affGetConstantSi affR
  evaluate (consume aff)
  let allTerms = [(v, e) | (v, e) <- dimCoeffs ++ paramCoeffs ++ divCoeffs, v /= 0]
      innerExpr = rebuildExprWithDivs allTerms constVal
  return $ FloorDiv innerExpr denom

extractMapDivs :: Isl.BasicMapRef -> Int -> Int -> Int -> IO [Expr MapIx]
extractMapDivs bmRef nIn nOut nParams = do
  let nDivs = BM.dim bmRef Isl.islDimDiv
  go 0 nDivs []
  where
    go i nDivs acc
      | i >= nDivs = return (reverse acc)
      | otherwise = do
          expr <- extractOneMapDiv bmRef nIn nOut nParams (reverse acc) i
          go (i + 1) nDivs (expr : acc)

extractOneMapDiv :: Isl.BasicMapRef -> Int -> Int -> Int -> [Expr MapIx] -> Int -> IO (Expr MapIx)
extractOneMapDiv bmRef nIn nOut nParams earlierDivs divIdx = do
  aff <- BM.c_getDiv bmRef (fromIntegral divIdx)
  let !affR = Isl.AffRef (Isl.unAff aff)
  denomVal <- Aff.c_getDenominatorVal affR
  let !denom = fromIntegral $ Val.getNumSi (Isl.ValRef (Isl.unVal denomVal))
  evaluate (consume denomVal)
  inCoeffs <- forM [0 .. nIn - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimIn (fromIntegral i)
    return (c, Ix (InDim i))
  outCoeffs <- forM [0 .. nOut - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimOut (fromIntegral i)
    return (c, Ix (OutDim i))
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimParam (fromIntegral i)
    return (c, Ix (MapParam i))
  divCoeffs <- forM (zip [0..] earlierDivs) $ \(i, divExpr) -> do
    c <- Aff.affGetCoefficientSi affR Isl.islDimDiv (fromIntegral (i :: Int))
    return (c, divExpr)
  constVal <- Aff.affGetConstantSi affR
  evaluate (consume aff)
  let allTerms = [(v, e) | (v, e) <- inCoeffs ++ outCoeffs ++ paramCoeffs ++ divCoeffs, v /= 0]
      innerExpr = rebuildExprWithDivs allTerms constVal
  return $ FloorDiv innerExpr denom

extractSetConstraint :: Int -> Int -> [Expr SetIx] -> Isl.ConstraintRef -> IO (Constraint SetIx)
extractSetConstraint nParams nDims divExprs cRef = do
  let !isEq = Constraint.isEquality cRef
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi cRef Isl.islDimParam i
    return (coeff, Ix (SetParam (fromIntegral i)))
  dimCoeffs <- forM [0 .. nDims - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi cRef Isl.islDimSet i
    return (coeff, Ix (SetDim (fromIntegral i)))
  divCoeffs <- forM (zip [0..] divExprs) $ \(i, divExpr) -> do
    coeff <- Constraint.constraintGetCoefficientSi cRef Isl.islDimDiv (fromIntegral (i :: Int))
    return (coeff, divExpr)
  constant <- Constraint.constraintGetConstantSi cRef
  let allTerms = [(v, e) | (v, e) <- paramCoeffs ++ dimCoeffs ++ divCoeffs, v /= 0]
      expr = rebuildExprWithDivs allTerms constant
  return $ if isEq
    then EqualityConstraint expr
    else InequalityConstraint expr

extractMapConstraint :: Int -> Int -> Int -> [Expr MapIx] -> Isl.ConstraintRef -> IO (Constraint MapIx)
extractMapConstraint nParams nIn nOut divExprs cRef = do
  let !isEq = Constraint.isEquality cRef
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi cRef Isl.islDimParam i
    return (coeff, Ix (MapParam i))
  inCoeffs <- forM [0 .. nIn - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi cRef Isl.islDimIn i
    return (coeff, Ix (InDim i))
  outCoeffs <- forM [0 .. nOut - 1] $ \j -> do
    coeff <- Constraint.constraintGetCoefficientSi cRef Isl.islDimOut j
    return (coeff, Ix (OutDim j))
  divCoeffs <- forM (zip [0..] divExprs) $ \(i, divExpr) -> do
    coeff <- Constraint.constraintGetCoefficientSi cRef Isl.islDimDiv (fromIntegral (i :: Int))
    return (coeff, divExpr)
  constant <- Constraint.constraintGetConstantSi cRef
  let allTerms = [(v, e) | (v, e) <- paramCoeffs ++ inCoeffs ++ outCoeffs ++ divCoeffs, v /= 0]
      expr = rebuildExprWithDivs allTerms constant
  return $ if isEq
    then EqualityConstraint expr
    else InequalityConstraint expr

-- =========================================================================
-- ISL construction (Haskell -> ISL Aff/Constraint)
-- =========================================================================

-- | Build an ISL Aff from an expression. Consumes the LocalSpace.
-- The dispatcher maps index references to (DimType, position).
exprToAff :: forall m ix. MonadIO m => (ix -> (Isl.DimType, Int)) -> Isl.LocalSpace %1 -> Expr ix -> IslT m Isl.Aff
exprToAff = unsafeCoerce goExpr where
  goExpr :: (ix -> (Isl.DimType, Int)) -> Isl.LocalSpace -> Expr ix -> IslT m Isl.Aff
  goExpr ixToDim ls expr = Isl.do
    result <- go expr
    freeM ls
    Isl.pure result
    where
      mkZero = unsafeIslFromIO $ \_ -> do
        ls' <- LS.c_copy ls
        Aff.c_zeroOnDomain ls'

      go (Ix ix) = Isl.do
        let (dimType, pos) = ixToDim ix
        aff <- mkZero
        Aff.setCoefficientSi aff dimType (fromIntegral pos) 1
      go (Constant k) = Isl.do
        aff <- mkZero
        Aff.setConstantSi aff (fromIntegral k)
      go (Mul k e) = Isl.do
        aff <- go e
        v <- Val.intFromSi k
        Aff.scaleVal aff v
      go (Add a b) = Isl.do
        affA <- go a
        affB <- go b
        Aff.add affA affB
      go (FloorDiv inner d) = Isl.do
        innerAff <- go inner
        scaled <- Aff.scaleDownUi innerAff (fromIntegral (abs d))
        floored <- Aff.floor scaled
        if d < 0
          then Aff.neg floored
          else Isl.pure floored

-- | Build an ISL Aff from a set expression. Consumes the LocalSpace.
exprToSetAff :: MonadIO m => Isl.LocalSpace %1 -> Expr SetIx -> IslT m Isl.Aff
exprToSetAff = exprToAff setIxToDim
  where
    setIxToDim (SetDim d)   = (Isl.islDimIn, d)
    setIxToDim (SetParam p) = (Isl.islDimParam, p)

-- | Build an ISL Aff from a map expression. Consumes the LocalSpace.
exprToMapAff :: MonadIO m => Isl.LocalSpace %1 -> Expr MapIx -> IslT m Isl.Aff
exprToMapAff = exprToAff mapIxToDim
  where
    mapIxToDim (InDim d)    = (Isl.islDimIn, d)
    mapIxToDim (OutDim d)   = (Isl.islDimOut, d)
    mapIxToDim (MapParam p) = (Isl.islDimParam, p)

constraintFromSetExpr :: forall m. MonadIO m
  => Isl.LocalSpace %1 -> Constraint SetIx -> IslT m Isl.Constraint
constraintFromSetExpr = unsafeCoerce go where
  go :: Isl.LocalSpace -> Constraint SetIx -> IslT m Isl.Constraint
  go ls (EqualityConstraint e) = Isl.do
    aff <- exprToSetAff ls e
    Constraint.equalityFromAff aff
  go ls (InequalityConstraint e) = Isl.do
    aff <- exprToSetAff ls e
    Constraint.inequalityFromAff aff

constraintFromMapExpr :: forall m. MonadIO m
  => Isl.LocalSpace %1 -> Constraint MapIx -> IslT m Isl.Constraint
constraintFromMapExpr = unsafeCoerce go where
  go :: Isl.LocalSpace -> Constraint MapIx -> IslT m Isl.Constraint
  go ls (EqualityConstraint e) = Isl.do
    aff <- exprToMapAff ls e
    Constraint.equalityFromAff aff
  go ls (InequalityConstraint e) = Isl.do
    aff <- exprToMapAff ls e
    Constraint.inequalityFromAff aff

addSetConstraint :: forall m. MonadIO m => Isl.BasicSet %1 -> Constraint SetIx -> IslT m Isl.BasicSet
addSetConstraint = unsafeCoerce goAdd where
  goAdd :: Isl.BasicSet -> Constraint SetIx -> IslT m Isl.BasicSet
  goAdd bs constraint = Isl.do
    let !(ref, bs') = borrow bs (\r -> r)
    sp <- BS.getSpace ref
    ls <- LS.fromSpace sp
    let e = case constraint of
          EqualityConstraint ex   -> ex
          InequalityConstraint ex -> ex
    if hasFloorDiv e
      then Isl.do
        c <- constraintFromSetExpr ls constraint
        BS.addConstraint bs' c
      else Isl.do
        let ex = case constraint of
              EqualityConstraint x   -> x
              InequalityConstraint x -> x
        emptyC <- case constraint of
          InequalityConstraint _ -> Constraint.inequalityAlloc ls
          EqualityConstraint _   -> Constraint.equalityAlloc ls
        let (coeffs, constant) = expandExpr ex
        linearPart <- Isl.foldM setCoeff emptyC coeffs
        finalC <- Constraint.setConstantSi linearPart (fromIntegral constant)
        BS.addConstraint bs' finalC
  setCoeff :: Isl.Constraint %1 -> (Integer, SetIx) -> IslT m Isl.Constraint
  setCoeff = unsafeCoerce go where
    go :: Isl.Constraint -> (Integer, SetIx) -> IslT m Isl.Constraint
    go constr (coeff, ix) =
      let (dimType, pos) = case ix of
            SetDim i   -> (Isl.islDimSet, i)
            SetParam i -> (Isl.islDimParam, i)
      in Constraint.setCoefficientSi
           constr dimType (fromIntegral pos) (fromIntegral coeff)

addMapConstraint :: forall m. MonadIO m => Isl.BasicMap %1 -> Constraint MapIx -> IslT m Isl.BasicMap
addMapConstraint = unsafeCoerce goAdd where
  goAdd :: Isl.BasicMap -> Constraint MapIx -> IslT m Isl.BasicMap
  goAdd bm constraint = Isl.do
    let !(ref, bm') = borrow bm (\r -> r)
    sp <- BM.getSpace ref
    ls <- LS.fromSpace sp
    let e = case constraint of
          EqualityConstraint ex   -> ex
          InequalityConstraint ex -> ex
    let !nIn = BM.dim ref Isl.islDimIn
    if hasFloorDiv e
      then Isl.do
        freeM ls  -- unused in FloorDiv path (wrap delegates to addSetConstraint)
        let toSetIx (InDim i)    = SetDim i
            toSetIx (OutDim j)   = SetDim (nIn + j)
            toSetIx (MapParam k) = SetParam k
            setConstraint = case constraint of
              EqualityConstraint ex   -> EqualityConstraint (fmap toSetIx ex)
              InequalityConstraint ex -> InequalityConstraint (fmap toSetIx ex)
        wrapped <- BM.wrap bm'
        wrapped' <- addSetConstraint wrapped setConstraint
        BS.unwrap wrapped'
      else Isl.do
        let ex = case constraint of
              EqualityConstraint x   -> x
              InequalityConstraint x -> x
        emptyC <- case constraint of
          InequalityConstraint _ -> Constraint.inequalityAlloc ls
          EqualityConstraint _   -> Constraint.equalityAlloc ls
        let (coeffs, constant) = expandExpr ex
        linearPart <- Isl.foldM setCoeff emptyC coeffs
        finalC <- Constraint.setConstantSi linearPart (fromIntegral constant)
        BM.addConstraint bm' finalC
  setCoeff :: Isl.Constraint %1 -> (Integer, MapIx) -> IslT m Isl.Constraint
  setCoeff = unsafeCoerce go where
    go :: Isl.Constraint -> (Integer, MapIx) -> IslT m Isl.Constraint
    go constr (coeff, dim) =
      let (dimType, pos) = case dim of
            InDim i    -> (Isl.islDimIn, i)
            OutDim j   -> (Isl.islDimOut, j)
            MapParam k -> (Isl.islDimParam, k)
      in Constraint.setCoefficientSi
           constr dimType (fromIntegral pos) (fromIntegral coeff)

-- =========================================================================
-- High-level construction (Conjunction -> raw ISL object)
-- =========================================================================

-- | Set parameter names on a Space, consuming and returning it.
setParamNamesLoop :: forall m. MonadIO m => Isl.Space %1 -> [String] -> IslT m Isl.Space
setParamNamesLoop = unsafeCoerce go where
  go :: Isl.Space -> [String] -> IslT m Isl.Space
  go sp names = Isl.foldM (\s (i, name) -> Space.setDimName s Isl.islDimParam i name) sp (zip [0..] names)

-- | Build an ISL BasicSet from parameter names, dimension count, and constraints.
buildBasicSet :: MonadIO m
  => [String]          -- ^ Parameter names (alphabetically sorted)
  -> Int               -- ^ Number of set dimensions
  -> Conjunction SetIx -- ^ Constraints
  -> IslT m Isl.BasicSet
buildBasicSet paramNames nDims (Conjunction constraints) = Isl.do
  space0 <- Space.setAlloc (fromIntegral (length paramNames)) (fromIntegral nDims)
  space <- setParamNamesLoop space0 paramNames
  univ <- BS.universe space
  Isl.foldM addSetConstraint univ constraints

-- | Build an ISL BasicMap from parameter names, dimension counts, and constraints.
buildBasicMap :: MonadIO m
  => [String]          -- ^ Parameter names
  -> Int               -- ^ Number of input dimensions
  -> Int               -- ^ Number of output dimensions
  -> Conjunction MapIx -- ^ Constraints
  -> IslT m Isl.BasicMap
buildBasicMap paramNames nIn nOut (Conjunction constraints) = Isl.do
  space0 <- Space.alloc (fromIntegral (length paramNames)) (fromIntegral nIn) (fromIntegral nOut)
  space <- setParamNamesLoop space0 paramNames
  univ <- BM.universe space
  Isl.foldM addMapConstraint univ constraints

-- | Build an ISL MultiAff from parameter names, dimension counts, and output expressions.
buildMultiAff :: forall m. MonadIO m
  => [String]          -- ^ Parameter names
  -> Int               -- ^ Number of input dimensions
  -> Int               -- ^ Number of output dimensions
  -> [Expr SetIx]      -- ^ Output expressions (length must equal nOut)
  -> IslT m Isl.MultiAff
buildMultiAff paramNames nIn nOut exprs = Isl.do
  space0 <- Space.alloc (fromIntegral (length paramNames)) (fromIntegral nIn) (fromIntegral nOut)
  space <- setParamNamesLoop space0 paramNames
  zma <- MA.zero space
  Isl.foldM setOneAff zma (zip [0..] exprs)
  where
    setOneAff :: Isl.MultiAff %1 -> (Int, Expr SetIx) -> IslT m Isl.MultiAff
    setOneAff = unsafeCoerce go where
      go :: Isl.MultiAff -> (Int, Expr SetIx) -> IslT m Isl.MultiAff
      go ma (j, expr) = Isl.do
        let !(ref, ma') = borrow ma (\r -> r)
        domSp <- MA.getDomainSpace ref
        ls <- LS.fromSpace domSp
        aff <- exprToSetAff ls expr
        MA.setAff ma' (fromIntegral j) aff

-- =========================================================================
-- High-level decomposition (raw ISL object -> pure representation)
-- =========================================================================

-- | Decompose an ISL BasicSet into its constraints.
decomposeBasicSet :: MonadIO m
  => Int               -- ^ Number of set dimensions
  -> Int               -- ^ Number of parameters
  -> Isl.BasicSetRef   -- ^ Borrowed reference
  -> IslT m (Conjunction SetIx)
decomposeBasicSet nDims nParams bsRef =
  unsafeIslFromIO $ \_ -> do
    divExprs <- extractSetDivs bsRef nDims nParams
    constraints <- BS.foreachConstraint bsRef $ \cRef ->
      extractSetConstraint nParams nDims divExprs cRef
    return (Conjunction constraints)

-- | Decompose an ISL BasicMap into its constraints.
decomposeBasicMap :: MonadIO m
  => Int               -- ^ Number of input dimensions
  -> Int               -- ^ Number of output dimensions
  -> Int               -- ^ Number of parameters
  -> Isl.BasicMapRef   -- ^ Borrowed reference
  -> IslT m (Conjunction MapIx)
decomposeBasicMap nIn nOut nParams bmRef =
  unsafeIslFromIO $ \_ -> do
    divExprs <- extractMapDivs bmRef nIn nOut nParams
    constraints <- BM.foreachConstraint bmRef $ \cRef ->
      extractMapConstraint nParams nIn nOut divExprs cRef
    return (Conjunction constraints)

-- | Decompose an ISL Set (union of basic sets) into a list of conjunctions.
decomposeSet :: MonadIO m
  => Int -> Int -> Isl.SetRef -> IslT m [Conjunction SetIx]
decomposeSet nDims nParams setRef =
  unsafeIslFromIO $ \_ ->
    S.foreachBasicSet setRef $ \bsRef -> do
      divExprs <- extractSetDivs bsRef nDims nParams
      constraints <- BS.foreachConstraint bsRef $ \cRef ->
        extractSetConstraint nParams nDims divExprs cRef
      return (Conjunction constraints)

-- | Decompose an ISL Map (union of basic maps) into a list of conjunctions.
decomposeMap :: MonadIO m
  => Int -> Int -> Int -> Isl.MapRef -> IslT m [Conjunction MapIx]
decomposeMap nIn nOut nParams mapRef =
  unsafeIslFromIO $ \_ ->
    M.foreachBasicMap mapRef $ \bmRef -> do
      divExprs <- extractMapDivs bmRef nIn nOut nParams
      constraints <- BM.foreachConstraint bmRef $ \cRef ->
        extractMapConstraint nParams nIn nOut divExprs cRef
      return (Conjunction constraints)

-- | Decompose an ISL MultiAff into its output expressions.
decomposeMultiAff :: MonadIO m
  => Int -> Int -> Isl.MultiAffRef -> IslT m [Expr SetIx]
decomposeMultiAff nParams nIn maRef =
  unsafeIslFromIO $ \_ -> do
    let nOut = MA.dim maRef Isl.islDimOut
    forM [0 .. nOut - 1] $ \j -> do
      aff <- MA.multiAffGetAffCopy maRef (fromIntegral j)
      let affR = Isl.AffRef (Isl.unAff aff)
      expr <- extractAffExprIO nParams nIn affR
      evaluate (consume aff)
      return expr

-- =========================================================================
-- Named (value-level) representations
-- =========================================================================

-- | A set with its tuple name and parameter names as runtime values.
-- Used for multi-statement programs where the tuple name identifies the statement.
data NamedSet = NamedSet
  { nsName   :: !(Maybe String)
  , nsParams :: ![String]
  , nsNDims  :: !Int
  , nsConjs  :: ![Conjunction SetIx]
  } deriving (Show, Eq, Generic)

instance NFData NamedSet

-- | A map with domain/range tuple names and parameter names as runtime values.
data NamedMap = NamedMap
  { nmDomainName :: !(Maybe String)
  , nmRangeName  :: !(Maybe String)
  , nmParams     :: ![String]
  , nmNIn        :: !Int
  , nmNOut       :: !Int
  , nmConjs      :: ![Conjunction MapIx]
  } deriving (Show, Eq, Generic)

instance NFData NamedMap

-- =========================================================================
-- Union map bridge (NamedMap <-> ISL)
-- =========================================================================

-- | Run an IslT IO action in raw IO context (safe when the action ignores ctx).
runIO :: IslT IO a -> IO a
runIO (IslT f) = f (Ctx nullPtr)

-- | Build an ISL UnionMap from a NamedMap (pure representation).
buildUnionMapFromNamed :: MonadIO m => NamedMap -> IslT m Isl.UnionMap
buildUnionMapFromNamed nm = Isl.do
  let nParams = length (nmParams nm)
      nIn     = nmNIn nm
      nOut    = nmNOut nm
  case nmConjs nm of
    []     -> Isl.do
      sp <- Space.alloc (fromIntegral nParams) (fromIntegral nIn) (fromIntegral nOut)
      sp' <- setParamNamesLoop sp (nmParams nm)
      sp'' <- case nmDomainName nm of
                Just name -> Space.setTupleName sp' Isl.islDimIn name
                Nothing   -> Isl.pure sp'
      UM.emptySpace sp''
    (c:cs) -> Isl.do
      m0 <- buildOneMap nParams nIn nOut (nmParams nm) (nmDomainName nm) c
      um0 <- UM.fromMap m0
      Isl.foldM (\acc conj -> Isl.do
        m' <- buildOneMap nParams nIn nOut (nmParams nm) (nmDomainName nm) conj
        um' <- UM.fromMap m'
        UM.union acc um') um0 cs
  where
    buildOneMap nP nI nO paramNames domName (Conjunction constraints) = Isl.do
      space0 <- Space.alloc (fromIntegral nP) (fromIntegral nI) (fromIntegral nO)
      space1 <- setParamNamesLoop space0 paramNames
      space2 <- case domName of
                  Just name -> Space.setTupleName space1 Isl.islDimIn name
                  Nothing   -> Isl.pure space1
      space3 <- case nmRangeName nm of
                  Just name -> Space.setTupleName space2 Isl.islDimOut name
                  Nothing   -> Isl.pure space2
      univ <- BM.universe space3
      bm <- Isl.foldM addMapConstraint univ constraints
      M.fromBasicMap bm


-- | Decompose an ISL UnionMap into per-space NamedMaps.
decomposeUnionMapNamed :: MonadIO m
  => Isl.UnionMapRef -> IslT m [NamedMap]
decomposeUnionMapNamed ref =
  unsafeIslFromIO $ \_ ->
    UM.foreachMap ref $ \mRef -> do
      space <- runIO $ M.getSpace mRef
      let spRef = Isl.SpaceRef (Isl.unSpace space)
          nIn  = Space.dim spRef Isl.islDimIn
          nOut = Space.dim spRef Isl.islDimOut
          nParams = Space.dim spRef Isl.islDimParam
      domainName <- Space.spaceGetTupleName space Isl.islDimIn
      rangeName  <- Space.spaceGetTupleName space Isl.islDimOut
      paramNames <- forM [0 .. nParams - 1] $ \i ->
        Space.spaceGetDimName space Isl.islDimParam i
      evaluate (consume space)
      conjunctions <- M.foreachBasicMap mRef $ \bmRef -> do
        divExprs <- extractMapDivs bmRef nIn nOut nParams
        constraints <- BM.foreachConstraint bmRef $ \cRef ->
          extractMapConstraint nParams nIn nOut divExprs cRef
        return (Conjunction constraints)
      return NamedMap
        { nmDomainName = domainName
        , nmRangeName  = rangeName
        , nmParams     = catMaybes paramNames
        , nmNIn        = nIn
        , nmNOut       = nOut
        , nmConjs      = conjunctions
        }
