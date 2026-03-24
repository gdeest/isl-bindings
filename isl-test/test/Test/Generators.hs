module Test.Generators where

import Test.QuickCheck

import Isl.HighLevel.Constraints

-- | Newtype wrappers fixing dimension counts for QuickCheck.
newtype Conj2 = Conj2 (Conjunction SetIx) deriving (Show)
newtype Conj3 = Conj3 (Conjunction SetIx) deriving (Show)
newtype MapConj11 = MapConj11 (Conjunction MapIx) deriving (Show)
newtype MapConj12 = MapConj12 (Conjunction MapIx) deriving (Show)

instance Arbitrary Conj2 where
  arbitrary = Conj2 <$> genBoundedConjunction 2

instance Arbitrary Conj3 where
  arbitrary = Conj3 <$> genBoundedConjunction 3

instance Arbitrary MapConj11 where
  arbitrary = MapConj11 <$> genMapConjunction 1 1

instance Arbitrary MapConj12 where
  arbitrary = MapConj12 <$> genMapConjunction 1 2

-- | Generate a bounded conjunction for @nDims@ set dimensions.
-- Starts with 0 <= x_i <= 10 for each dimension, then adds 0-3
-- random affine inequality constraints with small coefficients.
genBoundedConjunction :: Int -> Gen (Conjunction SetIx)
genBoundedConjunction nDims = do
  let bounds = concatMap (\i ->
        [ InequalityConstraint (Ix (SetDim i))
        , InequalityConstraint (Add (Constant 10) (Mul (-1) (Ix (SetDim i))))
        ]) [0 .. nDims - 1]
  nExtra <- choose (0, 3)
  extras <- vectorOf nExtra (genInequalityConstraint nDims)
  return $ Conjunction (bounds ++ extras)

-- | Generate a random inequality constraint over @nDims@ SetIx-indexed dims.
genInequalityConstraint :: Int -> Gen (Constraint SetIx)
genInequalityConstraint nDims = do
  expr <- genExpr nDims
  return $ InequalityConstraint expr

-- | Generate a random affine expression: sum of c_i * x_i + constant,
-- with bounded coefficients.
genExpr :: Int -> Gen (Expr SetIx)
genExpr nDims = do
  coeffs <- mapM (\i -> do
    c <- choose (-5, 5)
    return (c, SetDim i)) [0 .. nDims - 1]
  constant <- choose (-10, 10)
  return $ rebuildExpr (filter (\(c, _) -> c /= 0) coeffs) constant

-- | Generate a bounded map conjunction for @nIn@ input and @nOut@ output dims.
genMapConjunction :: Int -> Int -> Gen (Conjunction MapIx)
genMapConjunction nIn nOut = do
  let inBounds = concatMap (\i ->
        [ InequalityConstraint (Ix (InDim i))
        , InequalityConstraint (Add (Constant 10) (Mul (-1) (Ix (InDim i))))
        ]) [0 .. nIn - 1]
      outBounds = concatMap (\j ->
        [ InequalityConstraint (Ix (OutDim j))
        , InequalityConstraint (Add (Constant 10) (Mul (-1) (Ix (OutDim j))))
        ]) [0 .. nOut - 1]
  nExtra <- choose (0, 2)
  extras <- vectorOf nExtra (genMapInequalityConstraint nIn nOut)
  return $ Conjunction (inBounds ++ outBounds ++ extras)

-- | Generate a random inequality constraint over map dimensions.
genMapInequalityConstraint :: Int -> Int -> Gen (Constraint MapIx)
genMapInequalityConstraint nIn nOut = do
  let allDims = map InDim [0 .. nIn - 1] ++ map OutDim [0 .. nOut - 1]
  coeffs <- mapM (\dim -> do
    c <- choose (-5, 5)
    return (c, dim)) allDims
  constant <- choose (-10, 10)
  return $ InequalityConstraint $
    rebuildExpr (filter (\(c, _) -> c /= 0) coeffs) constant
