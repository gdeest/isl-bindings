{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.Constraint.AutoGen where

import Control.Monad
import Data.Reflection
import Isl.Types

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall "isl_constraint_cmp_last_non_zero" c_cmpLastNonZero :: Constraint -> Constraint -> IO C.CInt


cmpLastNonZero :: (Given Ctx) => Constraint -> Constraint -> Int
cmpLastNonZero = \c1' c2' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      c1 <- (return) c1'
      c2 <- (return) c2'

      let ctx = given :: Ctx
      c_cmpLastNonZero c1 c2


foreign import ccall "isl_constraint_involves_dims" c_involvesDims :: Constraint -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt


involvesDims :: (Given Ctx) => Constraint -> DimType -> Int -> Int -> Int
involvesDims = \constraint' typ' first' n' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_involvesDims constraint typ first n


foreign import ccall "isl_constraint_plain_cmp" c_plainCmp :: Constraint -> Constraint -> IO C.CInt


plainCmp :: (Given Ctx) => Constraint -> Constraint -> Int
plainCmp = \c1' c2' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      c1 <- (return) c1'
      c2 <- (return) c2'

      let ctx = given :: Ctx
      c_plainCmp c1 c2


foreign import ccall "isl_constraint_get_ctx" c_getCtx :: Constraint -> IO Ctx


getCtx :: (Given Ctx) => Constraint -> Ctx
getCtx = \c' -> 
    unsafePerformIO $ (return) =<< do
      c <- (return) c'

      let ctx = given :: Ctx
      c_getCtx c


foreign import ccall "isl_constraint_dump" c_dump :: Constraint -> IO ()


dump :: (Given Ctx) => Constraint -> ()
dump = \c' -> 
    unsafePerformIO $ (return) =<< do
      c <- (return) c'

      let ctx = given :: Ctx
      c_dump c


foreign import ccall "isl_constraint_get_dim_name" c_getDimName :: Constraint -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => Constraint -> DimType -> Int -> String
getDimName = \constraint' typ' pos' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName constraint typ pos


foreign import ccall "isl_constraint_is_div_constraint" c_isDivConstraint :: Constraint -> IO C.CBool


isDivConstraint :: (Given Ctx) => Constraint -> Bool
isDivConstraint = \constraint' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_isDivConstraint constraint


foreign import ccall "isl_constraint_is_equal" c_isEqual :: Constraint -> Constraint -> IO C.CBool


isEqual :: (Given Ctx) => Constraint -> Constraint -> Bool
isEqual = \constraint1' constraint2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      constraint1 <- (return) constraint1'
      constraint2 <- (return) constraint2'

      let ctx = given :: Ctx
      c_isEqual constraint1 constraint2


foreign import ccall "isl_constraint_is_equality" c_isEquality :: Constraint -> IO C.CBool


isEquality :: (Given Ctx) => Constraint -> Bool
isEquality = \constraint' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_isEquality constraint


foreign import ccall "isl_constraint_is_lower_bound" c_isLowerBound :: Constraint -> DimType -> C.CUInt -> IO C.CBool


isLowerBound :: (Given Ctx) => Constraint -> DimType -> Int -> Bool
isLowerBound = \constraint' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_isLowerBound constraint typ pos


foreign import ccall "isl_constraint_is_upper_bound" c_isUpperBound :: Constraint -> DimType -> C.CUInt -> IO C.CBool


isUpperBound :: (Given Ctx) => Constraint -> DimType -> Int -> Bool
isUpperBound = \constraint' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_isUpperBound constraint typ pos


foreign import ccall "isl_constraint_get_space" c_getSpace :: Constraint -> IO Space


getSpace :: (Given Ctx) => Constraint -> Space
getSpace = \constraint' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_getSpace constraint


foreign import ccall "isl_constraint_get_coefficient_val" c_getCoefficientVal :: Constraint -> DimType -> C.CInt -> IO Val


getCoefficientVal :: (Given Ctx) => Constraint -> DimType -> Int -> Val
getCoefficientVal = \constraint' typ' pos' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getCoefficientVal constraint typ pos


foreign import ccall "isl_constraint_get_constant_val" c_getConstantVal :: Constraint -> IO Val


getConstantVal :: (Given Ctx) => Constraint -> Val
getConstantVal = \constraint' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_getConstantVal constraint


foreign import ccall "isl_constraint_get_aff" c_getAff :: Constraint -> IO Aff


getAff :: (Given Ctx) => Constraint -> Aff
getAff = \constraint' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_getAff constraint


foreign import ccall "isl_constraint_get_bound" c_getBound :: Constraint -> DimType -> C.CInt -> IO Aff


getBound :: (Given Ctx) => Constraint -> DimType -> Int -> Aff
getBound = \constraint' typ' pos' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getBound constraint typ pos


foreign import ccall "isl_constraint_get_div" c_getDiv :: Constraint -> C.CInt -> IO Aff


getDiv :: (Given Ctx) => Constraint -> Int -> Aff
getDiv = \constraint' pos' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDiv constraint pos


foreign import ccall "isl_constraint_get_local_space" c_getLocalSpace :: Constraint -> IO LocalSpace


getLocalSpace :: (Given Ctx) => Constraint -> LocalSpace
getLocalSpace = \constraint' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_getLocalSpace constraint


foreign import ccall "isl_constraint_alloc_equality" c_allocEquality :: LocalSpace -> IO Constraint


allocEquality :: (Given Ctx) => LocalSpace -> Constraint
allocEquality = \ls' -> 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_allocEquality ls


foreign import ccall "isl_constraint_alloc_inequality" c_allocInequality :: LocalSpace -> IO Constraint


allocInequality :: (Given Ctx) => LocalSpace -> Constraint
allocInequality = \ls' -> 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_allocInequality ls


foreign import ccall "isl_constraint_copy" c_copy :: Constraint -> IO Constraint


copy :: (Given Ctx) => Constraint -> Constraint
copy = \c' -> 
    unsafePerformIO $ (return) =<< do
      c <- (return) c'

      let ctx = given :: Ctx
      c_copy c


foreign import ccall "isl_constraint_negate" c_negate :: Constraint -> IO Constraint


negate :: (Given Ctx) => Constraint -> Constraint
negate = \constraint' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_negate constraint


foreign import ccall "isl_constraint_set_coefficient_si" c_setCoefficientSi :: Constraint -> DimType -> C.CInt -> C.CInt -> IO Constraint


setCoefficientSi :: (Given Ctx) => Constraint -> DimType -> Int -> Int -> Constraint
setCoefficientSi = \constraint' typ' pos' v' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return . fromIntegral) v'

      let ctx = given :: Ctx
      c_setCoefficientSi constraint typ pos v


foreign import ccall "isl_constraint_set_coefficient_val" c_setCoefficientVal :: Constraint -> DimType -> C.CInt -> Val -> IO Constraint


setCoefficientVal :: (Given Ctx) => Constraint -> DimType -> Int -> Val -> Constraint
setCoefficientVal = \constraint' typ' pos' v' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return) v'

      let ctx = given :: Ctx
      c_setCoefficientVal constraint typ pos v


foreign import ccall "isl_constraint_set_constant_si" c_setConstantSi :: Constraint -> C.CInt -> IO Constraint


setConstantSi :: (Given Ctx) => Constraint -> Int -> Constraint
setConstantSi = \constraint' v' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'
      v <- (return . fromIntegral) v'

      let ctx = given :: Ctx
      c_setConstantSi constraint v


foreign import ccall "isl_constraint_set_constant_val" c_setConstantVal :: Constraint -> Val -> IO Constraint


setConstantVal :: (Given Ctx) => Constraint -> Val -> Constraint
setConstantVal = \constraint' v' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'
      v <- (return) v'

      let ctx = given :: Ctx
      c_setConstantVal constraint v


foreign import ccall "isl_equality_alloc" c_equalityAlloc :: LocalSpace -> IO Constraint


equalityAlloc :: (Given Ctx) => LocalSpace -> Constraint
equalityAlloc = \ls' -> 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_equalityAlloc ls


foreign import ccall "isl_equality_from_aff" c_equalityFromAff :: Aff -> IO Constraint


equalityFromAff :: (Given Ctx) => Aff -> Constraint
equalityFromAff = \aff' -> 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_equalityFromAff aff


foreign import ccall "isl_inequality_alloc" c_inequalityAlloc :: LocalSpace -> IO Constraint


inequalityAlloc :: (Given Ctx) => LocalSpace -> Constraint
inequalityAlloc = \ls' -> 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_inequalityAlloc ls


foreign import ccall "isl_inequality_from_aff" c_inequalityFromAff :: Aff -> IO Constraint


inequalityFromAff :: (Given Ctx) => Aff -> Constraint
inequalityFromAff = \aff' -> 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_inequalityFromAff aff


