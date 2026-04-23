{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.Val.Generated where

import Isl.Types (DimType(..))
import Isl.Types.Raw
import Isl.Types.Internal (Consumable(..), Borrow(..), Dupable(..))
import Isl.Monad.Internal
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_val_eq_si" c_eqSi :: ValRef s_v -> C.CLong -> IO C.CInt

eqSi :: ValRef s_v -> Integer -> Int
eqSi v i =
    let !r = unsafePerformIO $ fromIntegral <$> c_eqSi v (fromIntegral i) in r


foreign import ccall "isl_val_gt_si" c_gtSi :: ValRef s_v -> C.CLong -> IO C.CInt

gtSi :: ValRef s_v -> Integer -> Int
gtSi v i =
    let !r = unsafePerformIO $ fromIntegral <$> c_gtSi v (fromIntegral i) in r


foreign import ccall "isl_val_n_abs_num_chunks" c_nAbsNumChunks :: ValRef s_v -> C.CULong -> IO C.CInt

nAbsNumChunks :: ValRef s_v -> Int -> Int
nAbsNumChunks v size =
    let !r = unsafePerformIO $ fromIntegral <$> c_nAbsNumChunks v (fromIntegral size) in r


foreign import ccall "isl_val_dump" c_dump :: ValRef s_v -> IO ()

dump :: ValRef s_v -> ()
dump v =
    let !r = unsafePerformIO $ c_dump v in r


foreign import ccall "isl_val_2exp" c_twoExp :: Val -> IO Val

twoExp :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
twoExp = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_twoExp v


foreign import ccall "isl_val_add_ui" c_addUi :: Val -> C.CULong -> IO Val

addUi :: forall m s_v1. MonadIO m => Val %1 -> Integer -> IslT m Val
addUi = unsafeCoerce go where
  go :: Val -> Integer -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_addUi v1 (fromIntegral v2)


foreign import ccall "isl_val_div_ui" c_divUi :: Val -> C.CULong -> IO Val

divUi :: forall m s_v1. MonadIO m => Val %1 -> Integer -> IslT m Val
divUi = unsafeCoerce go where
  go :: Val -> Integer -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_divUi v1 (fromIntegral v2)


foreign import ccall "isl_val_get_den_val" c_getDenVal :: ValRef s_v -> IO Val

getDenVal :: MonadIO m => ValRef s_v -> IslT m Val
getDenVal v =
    unsafeIslFromIO $ \_ -> c_getDenVal v


foreign import ccall "isl_val_int_from_ui" c_intFromUi :: Ctx -> C.CULong -> IO Val

intFromUi :: MonadIO m => Integer -> IslT m Val
intFromUi u =
    unsafeIslFromIO $ \ctx -> c_intFromUi ctx (fromIntegral u)


foreign import ccall "isl_val_mul_ui" c_mulUi :: Val -> C.CULong -> IO Val

mulUi :: forall m s_v1. MonadIO m => Val %1 -> Integer -> IslT m Val
mulUi = unsafeCoerce go where
  go :: Val -> Integer -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_mulUi v1 (fromIntegral v2)


foreign import ccall "isl_val_set_si" c_setSi :: Val -> C.CLong -> IO Val

setSi :: forall m s_v. MonadIO m => Val %1 -> Integer -> IslT m Val
setSi = unsafeCoerce go where
  go :: Val -> Integer -> IslT m Val
  go v i =
    unsafeIslFromIO $ \_ -> c_setSi v (fromIntegral i)


foreign import ccall "isl_val_sub_ui" c_subUi :: Val -> C.CULong -> IO Val

subUi :: forall m s_v1. MonadIO m => Val %1 -> Integer -> IslT m Val
subUi = unsafeCoerce go where
  go :: Val -> Integer -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_subUi v1 (fromIntegral v2)


foreign import ccall "isl_val_to_str" c_toStr :: ValRef s_v -> IO C.CString

toStr :: ValRef s_v -> String
toStr v =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr v in r


foreign import ccall "isl_val_cmp_si" c_cmpSi :: ValRef s_v -> C.CLong -> IO C.CInt

cmpSi :: ValRef s_v -> Integer -> Int
cmpSi v i =
    let !r = unsafePerformIO $ fromIntegral <$> c_cmpSi v (fromIntegral i) in r


foreign import ccall "isl_val_sgn" c_sgn :: ValRef s_v -> IO C.CInt

sgn :: ValRef s_v -> Int
sgn v =
    let !r = unsafePerformIO $ fromIntegral <$> c_sgn v in r


foreign import ccall "isl_val_get_den_si" c_getDenSi :: ValRef s_v -> IO C.CLong

getDenSi :: ValRef s_v -> Integer
getDenSi v =
    let !r = unsafePerformIO $ fromIntegral <$> c_getDenSi v in r


foreign import ccall "isl_val_get_num_si" c_getNumSi :: ValRef s_v -> IO C.CLong

getNumSi :: ValRef s_v -> Integer
getNumSi v =
    let !r = unsafePerformIO $ fromIntegral <$> c_getNumSi v in r


foreign import ccall "isl_val_abs_eq" c_absEq :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

absEq :: ValRef s_v1 -> ValRef s_v2 -> Bool
absEq v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_absEq v1 v2 in r


foreign import ccall "isl_val_eq" c_eq :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

eq :: ValRef s_v1 -> ValRef s_v2 -> Bool
eq v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_eq v1 v2 in r


foreign import ccall "isl_val_ge" c_ge :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

ge :: ValRef s_v1 -> ValRef s_v2 -> Bool
ge v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_ge v1 v2 in r


foreign import ccall "isl_val_gt" c_gt :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

gt :: ValRef s_v1 -> ValRef s_v2 -> Bool
gt v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_gt v1 v2 in r


foreign import ccall "isl_val_is_divisible_by" c_isDivisibleBy :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

isDivisibleBy :: ValRef s_v1 -> ValRef s_v2 -> Bool
isDivisibleBy v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isDivisibleBy v1 v2 in r


foreign import ccall "isl_val_is_infty" c_isInfty :: ValRef s_v -> IO C.CBool

isInfty :: ValRef s_v -> Bool
isInfty v =
    let !r = unsafePerformIO $ M.toBool <$> c_isInfty v in r


foreign import ccall "isl_val_is_int" c_isInt :: ValRef s_v -> IO C.CBool

isInt :: ValRef s_v -> Bool
isInt v =
    let !r = unsafePerformIO $ M.toBool <$> c_isInt v in r


foreign import ccall "isl_val_is_nan" c_isNan :: ValRef s_v -> IO C.CBool

isNan :: ValRef s_v -> Bool
isNan v =
    let !r = unsafePerformIO $ M.toBool <$> c_isNan v in r


foreign import ccall "isl_val_is_neg" c_isNeg :: ValRef s_v -> IO C.CBool

isNeg :: ValRef s_v -> Bool
isNeg v =
    let !r = unsafePerformIO $ M.toBool <$> c_isNeg v in r


foreign import ccall "isl_val_is_neginfty" c_isNeginfty :: ValRef s_v -> IO C.CBool

isNeginfty :: ValRef s_v -> Bool
isNeginfty v =
    let !r = unsafePerformIO $ M.toBool <$> c_isNeginfty v in r


foreign import ccall "isl_val_is_negone" c_isNegone :: ValRef s_v -> IO C.CBool

isNegone :: ValRef s_v -> Bool
isNegone v =
    let !r = unsafePerformIO $ M.toBool <$> c_isNegone v in r


foreign import ccall "isl_val_is_nonneg" c_isNonneg :: ValRef s_v -> IO C.CBool

isNonneg :: ValRef s_v -> Bool
isNonneg v =
    let !r = unsafePerformIO $ M.toBool <$> c_isNonneg v in r


foreign import ccall "isl_val_is_nonpos" c_isNonpos :: ValRef s_v -> IO C.CBool

isNonpos :: ValRef s_v -> Bool
isNonpos v =
    let !r = unsafePerformIO $ M.toBool <$> c_isNonpos v in r


foreign import ccall "isl_val_is_one" c_isOne :: ValRef s_v -> IO C.CBool

isOne :: ValRef s_v -> Bool
isOne v =
    let !r = unsafePerformIO $ M.toBool <$> c_isOne v in r


foreign import ccall "isl_val_is_pos" c_isPos :: ValRef s_v -> IO C.CBool

isPos :: ValRef s_v -> Bool
isPos v =
    let !r = unsafePerformIO $ M.toBool <$> c_isPos v in r


foreign import ccall "isl_val_is_rat" c_isRat :: ValRef s_v -> IO C.CBool

isRat :: ValRef s_v -> Bool
isRat v =
    let !r = unsafePerformIO $ M.toBool <$> c_isRat v in r


foreign import ccall "isl_val_is_zero" c_isZero :: ValRef s_v -> IO C.CBool

isZero :: ValRef s_v -> Bool
isZero v =
    let !r = unsafePerformIO $ M.toBool <$> c_isZero v in r


foreign import ccall "isl_val_le" c_le :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

le :: ValRef s_v1 -> ValRef s_v2 -> Bool
le v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_le v1 v2 in r


foreign import ccall "isl_val_lt" c_lt :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

lt :: ValRef s_v1 -> ValRef s_v2 -> Bool
lt v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_lt v1 v2 in r


foreign import ccall "isl_val_ne" c_ne :: ValRef s_v1 -> ValRef s_v2 -> IO C.CBool

ne :: ValRef s_v1 -> ValRef s_v2 -> Bool
ne v1 v2 =
    let !r = unsafePerformIO $ M.toBool <$> c_ne v1 v2 in r


foreign import ccall "isl_val_abs" c_abs :: Val -> IO Val

abs :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
abs = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_abs v


foreign import ccall "isl_val_add" c_add :: Val -> Val -> IO Val

add :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
add = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_add v1 v2


foreign import ccall "isl_val_ceil" c_ceil :: Val -> IO Val

ceil :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
ceil = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_ceil v


foreign import ccall "isl_val_div" c_div :: Val -> Val -> IO Val

div :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
div = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_div v1 v2


foreign import ccall "isl_val_floor" c_floor :: Val -> IO Val

floor :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
floor = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_floor v


foreign import ccall "isl_val_gcd" c_gcd :: Val -> Val -> IO Val

gcd :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
gcd = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_gcd v1 v2


foreign import ccall "isl_val_infty" c_infty :: Ctx -> IO Val

infty :: MonadIO m => IslT m Val
infty =
    unsafeIslFromIO $ \ctx -> c_infty ctx


foreign import ccall "isl_val_inv" c_inv :: Val -> IO Val

inv :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
inv = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_inv v


foreign import ccall "isl_val_max" c_max :: Val -> Val -> IO Val

max :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
max = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_max v1 v2


foreign import ccall "isl_val_min" c_min :: Val -> Val -> IO Val

min :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
min = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_min v1 v2


foreign import ccall "isl_val_mod" c_modulo :: Val -> Val -> IO Val

modulo :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
modulo = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_modulo v1 v2


foreign import ccall "isl_val_mul" c_mul :: Val -> Val -> IO Val

mul :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
mul = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_mul v1 v2


foreign import ccall "isl_val_nan" c_nan :: Ctx -> IO Val

nan :: MonadIO m => IslT m Val
nan =
    unsafeIslFromIO $ \ctx -> c_nan ctx


foreign import ccall "isl_val_neg" c_neg :: Val -> IO Val

neg :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
neg = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_neg v


foreign import ccall "isl_val_neginfty" c_neginfty :: Ctx -> IO Val

neginfty :: MonadIO m => IslT m Val
neginfty =
    unsafeIslFromIO $ \ctx -> c_neginfty ctx


foreign import ccall "isl_val_negone" c_negone :: Ctx -> IO Val

negone :: MonadIO m => IslT m Val
negone =
    unsafeIslFromIO $ \ctx -> c_negone ctx


foreign import ccall "isl_val_one" c_one :: Ctx -> IO Val

one :: MonadIO m => IslT m Val
one =
    unsafeIslFromIO $ \ctx -> c_one ctx


foreign import ccall "isl_val_pow2" c_pow2 :: Val -> IO Val

pow2 :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
pow2 = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_pow2 v


foreign import ccall "isl_val_sub" c_sub :: Val -> Val -> IO Val

sub :: forall m s_v1 s_v2. MonadIO m => Val %1 -> Val %1 -> IslT m Val
sub = unsafeCoerce go where
  go :: Val -> Val -> IslT m Val
  go v1 v2 =
    unsafeIslFromIO $ \_ -> c_sub v1 v2


foreign import ccall "isl_val_trunc" c_trunc :: Val -> IO Val

trunc :: forall m s_v. MonadIO m => Val %1 -> IslT m Val
trunc = unsafeCoerce go where
  go :: Val -> IslT m Val
  go v =
    unsafeIslFromIO $ \_ -> c_trunc v


foreign import ccall "isl_val_zero" c_zero :: Ctx -> IO Val

zero :: MonadIO m => IslT m Val
zero =
    unsafeIslFromIO $ \ctx -> c_zero ctx


foreign import ccall "isl_val_int_from_si" c_intFromSi :: Ctx -> C.CLong -> IO Val

intFromSi :: MonadIO m => Integer -> IslT m Val
intFromSi i =
    unsafeIslFromIO $ \ctx -> c_intFromSi ctx (fromIntegral i)


foreign import ccall "isl_val_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Val

readFromStr :: MonadIO m => String -> IslT m Val
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_val_free" c_free :: Val -> IO ()

instance Consumable Val where
  consume = unsafeCoerce c_free


foreign import ccall "isl_val_copy" c_copy :: Val -> IO Val

instance Dupable Val where
  dup = unsafeCoerce $ \x -> do
    copy <- c_copy x
    return (x, copy)


instance Borrow Val ValRef where
  borrow = unsafeCoerce $ \(Val ptr) f -> let !r = f (ValRef ptr) in (r, Val ptr)


