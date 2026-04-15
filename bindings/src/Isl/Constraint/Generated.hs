{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.Constraint.Generated where

import Isl.Types
import Isl.Types.Internal (Consumable(..), Borrow(..), Dupable(..))
import Isl.Monad.Internal
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_constraint_cmp_last_non_zero" c_cmpLastNonZero :: ConstraintRef -> ConstraintRef -> IO C.CInt

cmpLastNonZero :: ConstraintRef -> ConstraintRef -> Int
cmpLastNonZero c1 c2 =
    let !r = unsafePerformIO $ fromIntegral <$> c_cmpLastNonZero c1 c2 in r


foreign import ccall "isl_constraint_dim" c_dim :: ConstraintRef -> DimType -> IO C.CInt

dim :: ConstraintRef -> DimType -> Int
dim constraint typ =
    let !r = unsafePerformIO $ fromIntegral <$> c_dim constraint typ in r


foreign import ccall "isl_constraint_involves_dims" c_involvesDims :: ConstraintRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: ConstraintRef -> DimType -> Int -> Int -> Int
involvesDims constraint typ first n =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesDims constraint typ (fromIntegral first) (fromIntegral n) in r


foreign import ccall "isl_constraint_plain_cmp" c_plainCmp :: ConstraintRef -> ConstraintRef -> IO C.CInt

plainCmp :: ConstraintRef -> ConstraintRef -> Int
plainCmp c1 c2 =
    let !r = unsafePerformIO $ fromIntegral <$> c_plainCmp c1 c2 in r


foreign import ccall "isl_constraint_dump" c_dump :: ConstraintRef -> IO ()

dump :: ConstraintRef -> ()
dump c =
    let !r = unsafePerformIO $ c_dump c in r


foreign import ccall "isl_constraint_get_dim_name" c_getDimName :: ConstraintRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: ConstraintRef -> DimType -> Int -> String
getDimName constraint typ pos =
    let !r = unsafePerformIO $ C.peekCString =<< c_getDimName constraint typ (fromIntegral pos) in r


foreign import ccall "isl_constraint_is_div_constraint" c_isDivConstraint :: ConstraintRef -> IO C.CBool

isDivConstraint :: ConstraintRef -> Bool
isDivConstraint constraint =
    let !r = unsafePerformIO $ M.toBool <$> c_isDivConstraint constraint in r


foreign import ccall "isl_constraint_is_equal" c_isEqual :: ConstraintRef -> ConstraintRef -> IO C.CBool

isEqual :: ConstraintRef -> ConstraintRef -> Bool
isEqual constraint1 constraint2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isEqual constraint1 constraint2 in r


foreign import ccall "isl_constraint_is_equality" c_isEquality :: ConstraintRef -> IO C.CBool

isEquality :: ConstraintRef -> Bool
isEquality constraint =
    let !r = unsafePerformIO $ M.toBool <$> c_isEquality constraint in r


foreign import ccall "isl_constraint_is_lower_bound" c_isLowerBound :: ConstraintRef -> DimType -> C.CUInt -> IO C.CBool

isLowerBound :: ConstraintRef -> DimType -> Int -> Bool
isLowerBound constraint typ pos =
    let !r = unsafePerformIO $ M.toBool <$> c_isLowerBound constraint typ (fromIntegral pos) in r


foreign import ccall "isl_constraint_is_upper_bound" c_isUpperBound :: ConstraintRef -> DimType -> C.CUInt -> IO C.CBool

isUpperBound :: ConstraintRef -> DimType -> Int -> Bool
isUpperBound constraint typ pos =
    let !r = unsafePerformIO $ M.toBool <$> c_isUpperBound constraint typ (fromIntegral pos) in r


foreign import ccall "isl_constraint_get_space" c_getSpace :: ConstraintRef -> IO Space

getSpace :: MonadIO m => ConstraintRef -> IslT m Space
getSpace constraint =
    unsafeIslFromIO $ \_ -> c_getSpace constraint


foreign import ccall "isl_constraint_get_coefficient_val" c_getCoefficientVal :: ConstraintRef -> DimType -> C.CInt -> IO Val

getCoefficientVal :: MonadIO m => ConstraintRef -> DimType -> Int -> IslT m Val
getCoefficientVal constraint typ pos =
    unsafeIslFromIO $ \_ -> c_getCoefficientVal constraint typ (fromIntegral pos)


foreign import ccall "isl_constraint_get_constant_val" c_getConstantVal :: ConstraintRef -> IO Val

getConstantVal :: MonadIO m => ConstraintRef -> IslT m Val
getConstantVal constraint =
    unsafeIslFromIO $ \_ -> c_getConstantVal constraint


foreign import ccall "isl_constraint_get_aff" c_getAff :: ConstraintRef -> IO Aff

getAff :: MonadIO m => ConstraintRef -> IslT m Aff
getAff constraint =
    unsafeIslFromIO $ \_ -> c_getAff constraint


foreign import ccall "isl_constraint_get_bound" c_getBound :: ConstraintRef -> DimType -> C.CInt -> IO Aff

getBound :: MonadIO m => ConstraintRef -> DimType -> Int -> IslT m Aff
getBound constraint typ pos =
    unsafeIslFromIO $ \_ -> c_getBound constraint typ (fromIntegral pos)


foreign import ccall "isl_constraint_get_div" c_getDiv :: ConstraintRef -> C.CInt -> IO Aff

getDiv :: MonadIO m => ConstraintRef -> Int -> IslT m Aff
getDiv constraint pos =
    unsafeIslFromIO $ \_ -> c_getDiv constraint (fromIntegral pos)


foreign import ccall "isl_constraint_get_local_space" c_getLocalSpace :: ConstraintRef -> IO LocalSpace

getLocalSpace :: MonadIO m => ConstraintRef -> IslT m LocalSpace
getLocalSpace constraint =
    unsafeIslFromIO $ \_ -> c_getLocalSpace constraint


foreign import ccall "isl_constraint_alloc_equality" c_allocEquality :: LocalSpace -> IO Constraint

allocEquality :: forall m. MonadIO m => LocalSpace %1 -> IslT m Constraint
allocEquality = unsafeCoerce go where
  go :: LocalSpace -> IslT m Constraint
  go ls =
    unsafeIslFromIO $ \_ -> c_allocEquality ls


foreign import ccall "isl_constraint_alloc_inequality" c_allocInequality :: LocalSpace -> IO Constraint

allocInequality :: forall m. MonadIO m => LocalSpace %1 -> IslT m Constraint
allocInequality = unsafeCoerce go where
  go :: LocalSpace -> IslT m Constraint
  go ls =
    unsafeIslFromIO $ \_ -> c_allocInequality ls


foreign import ccall "isl_constraint_negate" c_negate :: Constraint -> IO Constraint

negate :: forall m. MonadIO m => Constraint %1 -> IslT m Constraint
negate = unsafeCoerce go where
  go :: Constraint -> IslT m Constraint
  go constraint =
    unsafeIslFromIO $ \_ -> c_negate constraint


foreign import ccall "isl_constraint_set_coefficient_si" c_setCoefficientSi :: Constraint -> DimType -> C.CInt -> C.CInt -> IO Constraint

setCoefficientSi :: forall m. MonadIO m => Constraint %1 -> DimType -> Int -> Int -> IslT m Constraint
setCoefficientSi = unsafeCoerce go where
  go :: Constraint -> DimType -> Int -> Int -> IslT m Constraint
  go constraint typ pos v =
    unsafeIslFromIO $ \_ -> c_setCoefficientSi constraint typ (fromIntegral pos) (fromIntegral v)


foreign import ccall "isl_constraint_set_coefficient_val" c_setCoefficientVal :: Constraint -> DimType -> C.CInt -> Val -> IO Constraint

setCoefficientVal :: forall m. MonadIO m => Constraint %1 -> DimType -> Int -> Val %1 -> IslT m Constraint
setCoefficientVal = unsafeCoerce go where
  go :: Constraint -> DimType -> Int -> Val -> IslT m Constraint
  go constraint typ pos v =
    unsafeIslFromIO $ \_ -> c_setCoefficientVal constraint typ (fromIntegral pos) v


foreign import ccall "isl_constraint_set_constant_si" c_setConstantSi :: Constraint -> C.CInt -> IO Constraint

setConstantSi :: forall m. MonadIO m => Constraint %1 -> Int -> IslT m Constraint
setConstantSi = unsafeCoerce go where
  go :: Constraint -> Int -> IslT m Constraint
  go constraint v =
    unsafeIslFromIO $ \_ -> c_setConstantSi constraint (fromIntegral v)


foreign import ccall "isl_constraint_set_constant_val" c_setConstantVal :: Constraint -> Val -> IO Constraint

setConstantVal :: forall m. MonadIO m => Constraint %1 -> Val %1 -> IslT m Constraint
setConstantVal = unsafeCoerce go where
  go :: Constraint -> Val -> IslT m Constraint
  go constraint v =
    unsafeIslFromIO $ \_ -> c_setConstantVal constraint v


foreign import ccall "isl_equality_alloc" c_equalityAlloc :: LocalSpace -> IO Constraint

equalityAlloc :: forall m. MonadIO m => LocalSpace %1 -> IslT m Constraint
equalityAlloc = unsafeCoerce go where
  go :: LocalSpace -> IslT m Constraint
  go ls =
    unsafeIslFromIO $ \_ -> c_equalityAlloc ls


foreign import ccall "isl_equality_from_aff" c_equalityFromAff :: Aff -> IO Constraint

equalityFromAff :: forall m. MonadIO m => Aff %1 -> IslT m Constraint
equalityFromAff = unsafeCoerce go where
  go :: Aff -> IslT m Constraint
  go aff =
    unsafeIslFromIO $ \_ -> c_equalityFromAff aff


foreign import ccall "isl_inequality_alloc" c_inequalityAlloc :: LocalSpace -> IO Constraint

inequalityAlloc :: forall m. MonadIO m => LocalSpace %1 -> IslT m Constraint
inequalityAlloc = unsafeCoerce go where
  go :: LocalSpace -> IslT m Constraint
  go ls =
    unsafeIslFromIO $ \_ -> c_inequalityAlloc ls


foreign import ccall "isl_inequality_from_aff" c_inequalityFromAff :: Aff -> IO Constraint

inequalityFromAff :: forall m. MonadIO m => Aff %1 -> IslT m Constraint
inequalityFromAff = unsafeCoerce go where
  go :: Aff -> IslT m Constraint
  go aff =
    unsafeIslFromIO $ \_ -> c_inequalityFromAff aff


foreign import ccall "isl_constraint_free" c_free :: Constraint -> IO ()

instance Consumable Constraint where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_constraint_copy" c_copy :: Constraint -> IO Constraint

instance Dupable Constraint where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow Constraint ConstraintRef where
  borrow = unsafeCoerce $ \(Constraint ptr) f -> let !r = f (ConstraintRef ptr) in (r, Constraint ptr)


