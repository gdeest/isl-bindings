{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Isl.HighLevel.Constraints where

import Control.DeepSeq (NFData)
import Control.Exception (evaluate)
import Control.Monad (forM, foldM)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import GHC.Generics (Generic)

import qualified Isl.Types as Isl
import Isl.Types (Consumable(..), Borrow(..))
import Isl.Monad (IslT, unsafeIslFromIO)
import qualified Isl.Aff as Aff
import qualified Isl.BasicSet as BS
import qualified Isl.BasicMap as BM
import qualified Isl.Constraint as Constraint
import qualified Isl.LocalSpace as LS
import qualified Isl.Space as Space
import qualified Isl.Val as Val

-- | Dimension index for set constraints, distinguishing dimensions from parameters.
data SetIx = SetDim !Int | SetParam !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData SetIx

-- | Dimension index for map constraints, distinguishing input, output, and parameters.
data MapIx = InDim !Int | OutDim !Int | MapParam !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData MapIx

-- | Affine expressions (with floor division for existentials).
data Expr ix
  = Ix ix                        -- ^ Variable reference
  | Constant Integer             -- ^ Integer constant
  | Mul Integer (Expr ix)        -- ^ Scalar multiplication
  | Add (Expr ix) (Expr ix)      -- ^ Addition
  | FloorDiv (Expr ix) Integer   -- ^ @floor(expr / d)@ — from ISL existentials
  deriving (Generic, Functor)

-- | @modExpr a b@ = @a mod b@, expressed as @a - b * floor(a / b)@.
-- No new constructor needed — composes from existing 'FloorDiv'.
modExpr :: Expr ix -> Integer -> Expr ix
modExpr a b = Add a (Mul (-b) (FloorDiv a b))

instance NFData ix => NFData (Expr ix)

infixl 4 -:
infixl 4 +:
infixr 5 *:

infix 3 ==:
infix 3 <=:
infix 3 >=:

infixl 2 &&:


(&&:) :: ToConjunction c => c ix -> Constraint ix -> Conjunction ix
(&&:) conj constr = Conjunction (constr:cs)
  where Conjunction cs = toConjunction conj

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

(<=:) :: Expr ix -> Expr ix -> Constraint ix
e1 <=: e2 = InequalityConstraint $ Add e2 (Mul (-1) e1)

(>=:) :: Expr ix -> Expr ix -> Constraint ix
e1 >=: e2 = e2 <=: e1

(==:) :: Expr ix -> Expr ix -> Constraint ix
e1 ==: e2 = EqualityConstraint $ Add e2 (Mul (-1) e1)


-- | Affine equality / inequality constraints, with variable type 'ix'.
--
-- While equality constraints can be represented as a pair of inequality
-- constraints, they get special treatment in Isl for performance reasons.
data Constraint ix
  = EqualityConstraint (Expr ix)
    -- ^ 'EqualityConstraint e' represents the constraint 'e = 0'.
  | InequalityConstraint (Expr ix)
    -- ^ 'InequalityConstraint e' represents the constraint 'e >= 0'.
  deriving (Generic)

instance NFData ix => NFData (Constraint ix)

-- | Represents a conjunction of constraints, defining a single convex
-- polyhedron.
newtype Conjunction ix = Conjunction [Constraint ix]
  deriving (Generic)

instance NFData ix => NFData (Conjunction ix)

deriving instance Show ix => Show (Expr ix)
deriving instance Show ix => Show (Constraint ix)
deriving instance Show ix => Show (Conjunction ix)

deriving instance Eq ix => Eq (Expr ix)
deriving instance Eq ix => Eq (Constraint ix)
deriving instance Eq ix => Eq (Conjunction ix)

class ToConjunction (c :: Type -> Type) where
  toConjunction :: c ix -> Conjunction ix

instance ToConjunction Conjunction where
  toConjunction = id

instance ToConjunction Constraint where
  toConjunction = Conjunction . pure

-- | Legacy alias — use 'MapIx' for new code.
type MapDim = MapIx

-- | Reconstruct an 'Expr' from a list of (coefficient, variable) pairs and
-- a constant offset. Inverse of 'expandExpr'.
--
-- @expandExpr (rebuildExpr coeffs c) ≈ (coeffs, c)@ (modulo zero-coeff filtering)
rebuildExpr :: [(Integer, ix)] -> Integer -> Expr ix
rebuildExpr coeffs constant =
  let terms = [if c == 1 then Ix i else Mul c (Ix i) | (c, i) <- coeffs, c /= 0]
      constTerm = [Constant constant | constant /= 0]
      allTerms = terms ++ constTerm
  in case allTerms of
    []     -> Constant 0
    [t]    -> t
    (t:ts) -> foldl Add t ts

-- | Reconstruct an 'Expr' from (coefficient, expression) pairs and a constant.
-- Unlike 'rebuildExpr', this handles arbitrary sub-expressions (e.g. FloorDiv)
-- as coefficient targets, not just plain variables.
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

-- | Expands an affine expression to a set of coefficients / variable pairs,
-- plus a constant offset.
--
-- Variables are guaranteed to appear only once in the result, and increasing
-- order.
--
-- Crashes on 'FloorDiv' — use 'exprToSetAff' or 'exprToMapAff' for
-- expressions containing floor division.
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
expandExpr (FloorDiv _ _) = error "expandExpr: FloorDiv cannot be linearized — use exprToSetAff/exprToMapAff for constraints with floor division"

-- | Check whether an expression contains any 'FloorDiv' sub-expressions.
hasFloorDiv :: Expr ix -> Bool
hasFloorDiv (Ix _)          = False
hasFloorDiv (Constant _)    = False
hasFloorDiv (Mul _ e)       = hasFloorDiv e
hasFloorDiv (Add a b)       = hasFloorDiv a || hasFloorDiv b
hasFloorDiv (FloorDiv _ _)  = True

-- | Check whether any constraint in a list contains FloorDiv.
conjunctionHasFloorDiv :: [Constraint ix] -> Bool
conjunctionHasFloorDiv = any go
  where
    go (EqualityConstraint e)   = hasFloorDiv e
    go (InequalityConstraint e) = hasFloorDiv e

-- | Extract an affine expression from a raw ISL AffRef as a value-level 'Expr SetIx'.
-- Uses isl_dim_in for input dims and isl_dim_param for params.
-- Runs in IO; the AffRef is borrowed (not consumed).
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

-- | Extract an affine expression from a raw ISL AffRef as a value-level 'Expr SetIx'.
-- Uses isl_dim_in for set dims and isl_dim_param for params.
--
-- NOTE: ISL's isl_aff uses isl_dim_in for dimension coefficients, even when
-- created from a set's local space. Using isl_dim_set (== isl_dim_out) would
-- trigger "output/set dimension does not have a coefficient" warnings.
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
-- Div-aware extraction from ISL objects
-- =========================================================================

-- | Extract all div definitions from a BasicSet, resolving forward references.
-- Div k may depend on divs 0..k-1 (forward-referencing chain), so we process
-- in order and substitute earlier div expressions into later ones.
-- Returns a list of 'Expr SetIx' indexed by div position.
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

-- | Extract a single div definition, substituting earlier div expressions.
-- ISL quirk: isl_aff from basic_set_get_div uses isl_dim_in for set dimensions.
extractOneSetDiv :: Isl.BasicSetRef -> Int -> Int -> [Expr SetIx] -> Int -> IO (Expr SetIx)
extractOneSetDiv bsRef nDims nParams earlierDivs divIdx = do
  aff <- BS.c_getDiv bsRef (fromIntegral divIdx)
  let !affR = Isl.AffRef (Isl.unAff aff)
  denomVal <- Aff.c_getDenominatorVal affR
  let denom = fromIntegral $ Val.getNumSi (Isl.ValRef (Isl.unVal denomVal))
  evaluate (consume denomVal)
  -- ISL quirk: isl_aff from basic_set_get_div uses isl_dim_in for set dimensions
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

-- | Extract all div definitions from a BasicMap, resolving forward references.
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

-- | Extract a single map div definition, substituting earlier div expressions.
extractOneMapDiv :: Isl.BasicMapRef -> Int -> Int -> Int -> [Expr MapIx] -> Int -> IO (Expr MapIx)
extractOneMapDiv bmRef nIn nOut nParams earlierDivs divIdx = do
  aff <- BM.c_getDiv bmRef (fromIntegral divIdx)  -- IO Aff
  let !affR = Isl.AffRef (Isl.unAff aff)
  denomVal <- Aff.c_getDenominatorVal affR
  let denom = fromIntegral $ Val.getNumSi (Isl.ValRef (Isl.unVal denomVal))
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

-- | Extract a set constraint, reading div dimension coefficients and substituting
-- with resolved div expressions. Properly handles existential variables.
extractSetConstraint :: Int -> Int -> [Expr SetIx] -> Isl.Constraint -> IO (Constraint SetIx)
extractSetConstraint nParams nDims divExprs c = do
  let !cRef = Isl.ConstraintRef (Isl.unConstraint c)
      !isEq = Constraint.isEquality cRef
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi c Isl.islDimParam i
    return (coeff, Ix (SetParam (fromIntegral i)))
  dimCoeffs <- forM [0 .. nDims - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi c Isl.islDimSet i
    return (coeff, Ix (SetDim (fromIntegral i)))
  divCoeffs <- forM (zip [0..] divExprs) $ \(i, divExpr) -> do
    coeff <- Constraint.constraintGetCoefficientSi c Isl.islDimDiv (fromIntegral (i :: Int))
    return (coeff, divExpr)
  constant <- Constraint.constraintGetConstantSi c
  let allTerms = [(v, e) | (v, e) <- paramCoeffs ++ dimCoeffs ++ divCoeffs, v /= 0]
      expr = rebuildExprWithDivs allTerms constant
  return $ if isEq
    then EqualityConstraint expr
    else InequalityConstraint expr

-- | Extract a map constraint with div substitution.
extractMapConstraint :: Int -> Int -> Int -> [Expr MapIx] -> Isl.Constraint -> IO (Constraint MapIx)
extractMapConstraint nParams nIn nOut divExprs c = do
  let !cRef = Isl.ConstraintRef (Isl.unConstraint c)
      !isEq = Constraint.isEquality cRef
  paramCoeffs <- forM [0 .. nParams - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi c Isl.islDimParam i
    return (coeff, Ix (MapParam i))
  inCoeffs <- forM [0 .. nIn - 1] $ \i -> do
    coeff <- Constraint.constraintGetCoefficientSi c Isl.islDimIn i
    return (coeff, Ix (InDim i))
  outCoeffs <- forM [0 .. nOut - 1] $ \j -> do
    coeff <- Constraint.constraintGetCoefficientSi c Isl.islDimOut j
    return (coeff, Ix (OutDim j))
  divCoeffs <- forM (zip [0..] divExprs) $ \(i, divExpr) -> do
    coeff <- Constraint.constraintGetCoefficientSi c Isl.islDimDiv (fromIntegral (i :: Int))
    return (coeff, divExpr)
  constant <- Constraint.constraintGetConstantSi c
  let allTerms = [(v, e) | (v, e) <- paramCoeffs ++ inCoeffs ++ outCoeffs ++ divCoeffs, v /= 0]
      expr = rebuildExprWithDivs allTerms constant
  return $ if isEq
    then EqualityConstraint expr
    else InequalityConstraint expr

-- =========================================================================
-- Div-aware reconstruction: Expr → ISL Aff
-- =========================================================================

-- | Build an ISL Aff from a set expression, handling FloorDiv recursively.
-- Uses isl_aff_floor + isl_aff_scale_down for floor division terms.
-- The local space pointer is borrowed (copied internally as needed).
--
-- NOTE: uses 'islDimIn' (not 'islDimSet') because ISL's isl_aff is always
-- internally a "map aff" where coefficients live on isl_dim_in.
-- isl_dim_set == isl_dim_out, which would trigger warnings from ISL.
exprToSetAff :: MonadIO m => Isl.LocalSpace -> Expr SetIx -> IslT m Isl.Aff
exprToSetAff ls = exprToAff Isl.islDimIn ls

-- | Build an ISL Aff from an expression for a multi-aff output dimension.
-- Like 'exprToSetAff' but uses 'islDimIn' for dimensions (multi-aff domain context).
exprToMultiAffAff :: MonadIO m => Isl.LocalSpace -> Expr SetIx -> IslT m Isl.Aff
exprToMultiAffAff ls = exprToAff Isl.islDimIn ls

-- | Build an ISL Aff from a SetIx expression with a configurable dim type.
-- Factored out of exprToSetAff/exprToMultiAffAff — the only difference
-- between set and multi-aff contexts is how SetDim indices map to ISL dim types.
exprToAff :: MonadIO m => Isl.DimType -> Isl.LocalSpace -> Expr SetIx -> IslT m Isl.Aff
exprToAff setDimType ls = go
  where
    -- Each zeroOnDomain consumes a LocalSpace, so copy for each leaf.
    mkZero = unsafeIslFromIO $ \_ -> do
      ls' <- LS.c_copy ls
      Aff.c_zeroOnDomain ls'

    go (Ix (SetDim d)) = do
      aff <- mkZero
      Aff.setCoefficientSi aff setDimType (fromIntegral d) 1
    go (Ix (SetParam p)) = do
      aff <- mkZero
      Aff.setCoefficientSi aff Isl.islDimParam (fromIntegral p) 1
    go (Constant k) = do
      aff <- mkZero
      Aff.setConstantSi aff (fromIntegral k)
    go (Mul k e) = do
      aff <- go e
      v <- Val.intFromSi k
      Aff.scaleVal aff v
    go (Add a b) = do
      affA <- go a
      affB <- go b
      Aff.add affA affB
    go (FloorDiv inner d) = do
      innerAff <- go inner
      scaled <- Aff.scaleDownUi innerAff (fromIntegral (abs d))
      floored <- Aff.floor scaled
      if d < 0
        then Aff.neg floored
        else return floored

-- | Build an ISL Aff from a map expression, handling FloorDiv recursively.
-- Map dimensions are split: InDim → isl_dim_in, OutDim → isl_dim_out.
exprToMapAff :: MonadIO m => Isl.LocalSpace -> Expr MapIx -> IslT m Isl.Aff
exprToMapAff ls = go
  where
    mkZero = unsafeIslFromIO $ \_ -> do
      ls' <- LS.c_copy ls
      Aff.c_zeroOnDomain ls'

    go (Ix (InDim d)) = do
      aff <- mkZero
      Aff.setCoefficientSi aff Isl.islDimIn (fromIntegral d) 1
    go (Ix (OutDim d)) = do
      aff <- mkZero
      Aff.setCoefficientSi aff Isl.islDimOut (fromIntegral d) 1
    go (Ix (MapParam p)) = do
      aff <- mkZero
      Aff.setCoefficientSi aff Isl.islDimParam (fromIntegral p) 1
    go (Constant k) = do
      aff <- mkZero
      Aff.setConstantSi aff (fromIntegral k)
    go (Mul k e) = do
      aff <- go e
      v <- Val.intFromSi k
      Aff.scaleVal aff v
    go (Add a b) = do
      affA <- go a
      affB <- go b
      Aff.add affA affB
    go (FloorDiv inner d) = do
      innerAff <- go inner
      scaled <- Aff.scaleDownUi innerAff (fromIntegral (abs d))
      floored <- Aff.floor scaled
      if d < 0
        then Aff.neg floored
        else return floored

-- | Build an ISL constraint from a set expression that may contain FloorDiv.
-- Uses isl_equality_from_aff / isl_inequality_from_aff which properly
-- introduces existential (div) dimensions in the local space.
constraintFromSetExpr :: MonadIO m
  => Isl.LocalSpace -> Constraint SetIx -> IslT m Isl.Constraint
constraintFromSetExpr ls (EqualityConstraint e) = do
  aff <- exprToSetAff ls e
  Constraint.equalityFromAff aff
constraintFromSetExpr ls (InequalityConstraint e) = do
  aff <- exprToSetAff ls e
  Constraint.inequalityFromAff aff

-- | Build an ISL constraint from a map expression that may contain FloorDiv.
constraintFromMapExpr :: MonadIO m
  => Isl.LocalSpace -> Constraint MapIx -> IslT m Isl.Constraint
constraintFromMapExpr ls (EqualityConstraint e) = do
  aff <- exprToMapAff ls e
  Constraint.equalityFromAff aff
constraintFromMapExpr ls (InequalityConstraint e) = do
  aff <- exprToMapAff ls e
  Constraint.inequalityFromAff aff

-- | Add a set constraint (possibly containing FloorDiv) to a BasicSet.
-- If the constraint is purely linear, uses the fast coefficient-setting path.
-- If it contains FloorDiv, builds an Aff and uses equalityFromAff/inequalityFromAff.
addSetConstraint :: MonadIO m => Isl.BasicSet -> Constraint SetIx -> IslT m Isl.BasicSet
addSetConstraint bs constraint = do
  let !(ref, bs') = Isl.borrow bs (\r -> r)
  sp <- BS.getSpace ref
  ls <- LS.fromSpace sp
  let e = case constraint of
        EqualityConstraint ex   -> ex
        InequalityConstraint ex -> ex
  if hasFloorDiv e
    then do
      c <- constraintFromSetExpr ls constraint
      BS.addConstraint bs' c
    else do
      (emptyC, ex) <- case constraint of
        InequalityConstraint ex -> do
          co <- Constraint.inequalityAlloc ls
          return (co, ex)
        EqualityConstraint ex -> do
          co <- Constraint.equalityAlloc ls
          return (co, ex)
      let (coeffs, constant) = expandExpr ex
          setCoeff constr (coeff, ix) = do
            let (dimType, pos) = case ix of
                  SetDim i   -> (Isl.islDimSet, i)
                  SetParam i -> (Isl.islDimParam, i)
            Constraint.setCoefficientSi
              constr dimType (fromIntegral pos) (fromIntegral coeff)
      linearPart <- foldM setCoeff emptyC coeffs
      finalC <- Constraint.setConstantSi linearPart (fromIntegral constant)
      BS.addConstraint bs' finalC

-- | Add a map constraint (possibly containing FloorDiv) to a BasicMap.
--
-- For FloorDiv constraints, wraps the map to a set (combined domain×range
-- space), adds the constraint in set space where isl_aff_floor works,
-- then unwraps back to map space.
addMapConstraint :: MonadIO m => Isl.BasicMap -> Constraint MapIx -> IslT m Isl.BasicMap
addMapConstraint bm constraint = do
  let !(ref, bm') = Isl.borrow bm (\r -> r)
  sp <- BM.getSpace ref
  ls <- LS.fromSpace sp
  let e = case constraint of
        EqualityConstraint ex   -> ex
        InequalityConstraint ex -> ex
  let !nIn = BM.dim ref Isl.islDimIn
  if hasFloorDiv e
    then do
      -- isl_aff_floor requires a set-space domain. Wrap the basic map to a
      -- basic set in the product space [in_0..in_{ni-1}, out_0..out_{no-1}],
      -- remap MapIx → SetIx, add the constraint via the set path (which
      -- handles FloorDiv correctly), then unwrap back.
      let toSetIx (InDim i)    = SetDim i
          toSetIx (OutDim j)   = SetDim (nIn + j)
          toSetIx (MapParam k) = SetParam k
          setConstraint = case constraint of
            EqualityConstraint ex   -> EqualityConstraint (fmap toSetIx ex)
            InequalityConstraint ex -> InequalityConstraint (fmap toSetIx ex)
      wrapped <- BM.wrap bm'
      wrapped' <- addSetConstraint wrapped setConstraint
      BS.unwrap wrapped'
    else do
      (emptyC, ex) <- case constraint of
        InequalityConstraint ex -> do
          co <- Constraint.inequalityAlloc ls
          return (co, ex)
        EqualityConstraint ex -> do
          co <- Constraint.equalityAlloc ls
          return (co, ex)
      let (coeffs, constant) = expandExpr ex
          setCoeff constr (coeff, dim) = do
            let (dimType, pos) = case dim of
                  InDim i    -> (Isl.islDimIn, i)
                  OutDim j   -> (Isl.islDimOut, j)
                  MapParam k -> (Isl.islDimParam, k)
            Constraint.setCoefficientSi
              constr dimType (fromIntegral pos) (fromIntegral coeff)
      linearPart <- foldM setCoeff emptyC coeffs
      finalC <- Constraint.setConstantSi linearPart (fromIntegral constant)
      BM.addConstraint bm' finalC
