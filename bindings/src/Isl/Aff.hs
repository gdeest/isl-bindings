{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.Aff
  ( module Isl.Aff.Generated
    -- * Composite integer extraction
  , affGetCoefficientSi
  , affGetConstantSi
  ) where

import Foreign.C.Types
import Isl.Types
import Isl.Aff.Generated

foreign import ccall "isl_aff_get_coefficient_val"
  c_aff_get_coefficient_val :: AffRef -> CInt -> CInt -> IO Val

foreign import ccall "isl_aff_get_constant_val"
  c_aff_get_constant_val :: AffRef -> IO Val

foreign import ccall "isl_val_get_num_si"
  c_val_get_num_si :: Val -> IO CLong

foreign import ccall "isl_val_free"
  c_val_free :: Val -> IO ()

-- | Get the coefficient of a dimension in an Aff as an Integer.
-- Extracts the Val and frees it, returning just the numerator.
affGetCoefficientSi :: AffRef -> DimType -> Int -> IO Integer
affGetCoefficientSi aff (DimType dt) pos = do
  val <- c_aff_get_coefficient_val aff dt (fromIntegral pos)
  n <- fromIntegral <$> c_val_get_num_si val
  c_val_free val
  return n

-- | Get the constant term of an Aff as an Integer.
affGetConstantSi :: AffRef -> IO Integer
affGetConstantSi aff = do
  val <- c_aff_get_constant_val aff
  n <- fromIntegral <$> c_val_get_num_si val
  c_val_free val
  return n
