{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.Constraint
  ( module Isl.Constraint.Generated
    -- * Composite integer extraction
  , constraintGetCoefficientSi
  , constraintGetConstantSi
  ) where

import Foreign.C.Types
import Isl.Types
import Isl.Constraint.Generated

foreign import ccall "isl_constraint_get_coefficient_val"
  c_get_coefficient_val :: ConstraintRef -> CInt -> CInt -> IO Val

foreign import ccall "isl_constraint_get_constant_val"
  c_get_constant_val :: ConstraintRef -> IO Val

foreign import ccall "isl_val_get_num_si"
  c_val_get_num_si :: Val -> IO CLong

foreign import ccall "isl_val_free"
  c_val_free :: Val -> IO ()

-- | Get the coefficient of a dimension as an Integer. Extracts the Val
-- and frees it, returning just the numerator (assumes integer coefficient).
constraintGetCoefficientSi :: ConstraintRef -> DimType -> Int -> IO Integer
constraintGetCoefficientSi c (DimType dt) pos = do
  val <- c_get_coefficient_val c dt (fromIntegral pos)
  n <- fromIntegral <$> c_val_get_num_si val
  c_val_free val
  return n

-- | Get the constant term of a constraint as an Integer.
constraintGetConstantSi :: ConstraintRef -> IO Integer
constraintGetConstantSi c = do
  val <- c_get_constant_val c
  n <- fromIntegral <$> c_val_get_num_si val
  c_val_free val
  return n
