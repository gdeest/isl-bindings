{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.Val.AutoGen where

import Control.Monad
import Data.Reflection
import Isl.Types
import Debug.Trace

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall "isl_val_get_ctx" c_getCtx :: Val -> IO Ctx


getCtx :: (Given Ctx) => Val -> Ctx
getCtx = \val' -> trace "getCtx" $ 
    unsafePerformIO $ (return) =<< do
      val <- (return) val'

      let ctx = given :: Ctx
      c_getCtx val


foreign import ccall "isl_val_dump" c_dump :: Val -> IO ()


dump :: (Given Ctx) => Val -> ()
dump = \v' -> trace "dump" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_dump v


foreign import ccall "isl_val_2exp" c_twoExp :: Val -> IO Val


twoExp :: (Given Ctx) => Val -> Val
twoExp = \v' -> trace "twoExp" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_twoExp v


foreign import ccall "isl_val_copy" c_copy :: Val -> IO Val


copy :: (Given Ctx) => Val -> Val
copy = \v' -> trace "copy" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_copy v


foreign import ccall "isl_val_get_den_val" c_getDenVal :: Val -> IO Val


getDenVal :: (Given Ctx) => Val -> Val
getDenVal = \v' -> trace "getDenVal" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_getDenVal v


foreign import ccall "isl_val_to_str" c_toStr :: Val -> IO C.CString


toStr :: (Given Ctx) => Val -> String
toStr = \v' -> trace "toStr" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_toStr v


foreign import ccall "isl_val_sgn" c_sgn :: Val -> IO C.CInt


sgn :: (Given Ctx) => Val -> Int
sgn = \v' -> trace "sgn" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_sgn v


foreign import ccall "isl_val_abs_eq" c_absEq :: Val -> Val -> IO C.CBool


absEq :: (Given Ctx) => Val -> Val -> Bool
absEq = \v1' v2' -> trace "absEq" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_absEq v1 v2


foreign import ccall "isl_val_eq" c_eq :: Val -> Val -> IO C.CBool


eq :: (Given Ctx) => Val -> Val -> Bool
eq = \v1' v2' -> trace "eq" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_eq v1 v2


foreign import ccall "isl_val_ge" c_ge :: Val -> Val -> IO C.CBool


ge :: (Given Ctx) => Val -> Val -> Bool
ge = \v1' v2' -> trace "ge" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_ge v1 v2


foreign import ccall "isl_val_gt" c_gt :: Val -> Val -> IO C.CBool


gt :: (Given Ctx) => Val -> Val -> Bool
gt = \v1' v2' -> trace "gt" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_gt v1 v2


foreign import ccall "isl_val_is_divisible_by" c_isDivisibleBy :: Val -> Val -> IO C.CBool


isDivisibleBy :: (Given Ctx) => Val -> Val -> Bool
isDivisibleBy = \v1' v2' -> trace "isDivisibleBy" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_isDivisibleBy v1 v2


foreign import ccall "isl_val_is_infty" c_isInfty :: Val -> IO C.CBool


isInfty :: (Given Ctx) => Val -> Bool
isInfty = \v' -> trace "isInfty" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isInfty v


foreign import ccall "isl_val_is_int" c_isInt :: Val -> IO C.CBool


isInt :: (Given Ctx) => Val -> Bool
isInt = \v' -> trace "isInt" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isInt v


foreign import ccall "isl_val_is_nan" c_isNan :: Val -> IO C.CBool


isNan :: (Given Ctx) => Val -> Bool
isNan = \v' -> trace "isNan" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isNan v


foreign import ccall "isl_val_is_neg" c_isNeg :: Val -> IO C.CBool


isNeg :: (Given Ctx) => Val -> Bool
isNeg = \v' -> trace "isNeg" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isNeg v


foreign import ccall "isl_val_is_neginfty" c_isNeginfty :: Val -> IO C.CBool


isNeginfty :: (Given Ctx) => Val -> Bool
isNeginfty = \v' -> trace "isNeginfty" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isNeginfty v


foreign import ccall "isl_val_is_negone" c_isNegone :: Val -> IO C.CBool


isNegone :: (Given Ctx) => Val -> Bool
isNegone = \v' -> trace "isNegone" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isNegone v


foreign import ccall "isl_val_is_nonneg" c_isNonneg :: Val -> IO C.CBool


isNonneg :: (Given Ctx) => Val -> Bool
isNonneg = \v' -> trace "isNonneg" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isNonneg v


foreign import ccall "isl_val_is_nonpos" c_isNonpos :: Val -> IO C.CBool


isNonpos :: (Given Ctx) => Val -> Bool
isNonpos = \v' -> trace "isNonpos" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isNonpos v


foreign import ccall "isl_val_is_one" c_isOne :: Val -> IO C.CBool


isOne :: (Given Ctx) => Val -> Bool
isOne = \v' -> trace "isOne" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isOne v


foreign import ccall "isl_val_is_pos" c_isPos :: Val -> IO C.CBool


isPos :: (Given Ctx) => Val -> Bool
isPos = \v' -> trace "isPos" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isPos v


foreign import ccall "isl_val_is_rat" c_isRat :: Val -> IO C.CBool


isRat :: (Given Ctx) => Val -> Bool
isRat = \v' -> trace "isRat" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isRat v


foreign import ccall "isl_val_is_zero" c_isZero :: Val -> IO C.CBool


isZero :: (Given Ctx) => Val -> Bool
isZero = \v' -> trace "isZero" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_isZero v


foreign import ccall "isl_val_le" c_le :: Val -> Val -> IO C.CBool


le :: (Given Ctx) => Val -> Val -> Bool
le = \v1' v2' -> trace "le" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_le v1 v2


foreign import ccall "isl_val_lt" c_lt :: Val -> Val -> IO C.CBool


lt :: (Given Ctx) => Val -> Val -> Bool
lt = \v1' v2' -> trace "lt" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_lt v1 v2


foreign import ccall "isl_val_ne" c_ne :: Val -> Val -> IO C.CBool


ne :: (Given Ctx) => Val -> Val -> Bool
ne = \v1' v2' -> trace "ne" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_ne v1 v2


foreign import ccall "isl_val_abs" c_abs :: Val -> IO Val


abs :: (Given Ctx) => Val -> Val
abs = \v' -> trace "abs" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_abs v


foreign import ccall "isl_val_add" c_add :: Val -> Val -> IO Val


add :: (Given Ctx) => Val -> Val -> Val
add = \v1' v2' -> trace "add" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_add v1 v2


foreign import ccall "isl_val_ceil" c_ceil :: Val -> IO Val


ceil :: (Given Ctx) => Val -> Val
ceil = \v' -> trace "ceil" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_ceil v


foreign import ccall "isl_val_div" c_div :: Val -> Val -> IO Val


div :: (Given Ctx) => Val -> Val -> Val
div = \v1' v2' -> trace "div" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_div v1 v2


foreign import ccall "isl_val_floor" c_floor :: Val -> IO Val


floor :: (Given Ctx) => Val -> Val
floor = \v' -> trace "floor" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_floor v


foreign import ccall "isl_val_gcd" c_gcd :: Val -> Val -> IO Val


gcd :: (Given Ctx) => Val -> Val -> Val
gcd = \v1' v2' -> trace "gcd" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_gcd v1 v2


foreign import ccall "isl_val_infty" c_infty :: Ctx -> IO Val


infty :: (Given Ctx) => Val
infty =  trace "infty" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_infty ctx


foreign import ccall "isl_val_inv" c_inv :: Val -> IO Val


inv :: (Given Ctx) => Val -> Val
inv = \v' -> trace "inv" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_inv v


foreign import ccall "isl_val_max" c_max :: Val -> Val -> IO Val


max :: (Given Ctx) => Val -> Val -> Val
max = \v1' v2' -> trace "max" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_max v1 v2


foreign import ccall "isl_val_min" c_min :: Val -> Val -> IO Val


min :: (Given Ctx) => Val -> Val -> Val
min = \v1' v2' -> trace "min" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_min v1 v2


foreign import ccall "isl_val_mod" c_modulo :: Val -> Val -> IO Val


modulo :: (Given Ctx) => Val -> Val -> Val
modulo = \v1' v2' -> trace "modulo" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_modulo v1 v2


foreign import ccall "isl_val_mul" c_mul :: Val -> Val -> IO Val


mul :: (Given Ctx) => Val -> Val -> Val
mul = \v1' v2' -> trace "mul" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_mul v1 v2


foreign import ccall "isl_val_nan" c_nan :: Ctx -> IO Val


nan :: (Given Ctx) => Val
nan =  trace "nan" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_nan ctx


foreign import ccall "isl_val_neg" c_neg :: Val -> IO Val


neg :: (Given Ctx) => Val -> Val
neg = \v' -> trace "neg" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_neg v


foreign import ccall "isl_val_neginfty" c_neginfty :: Ctx -> IO Val


neginfty :: (Given Ctx) => Val
neginfty =  trace "neginfty" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_neginfty ctx


foreign import ccall "isl_val_negone" c_negone :: Ctx -> IO Val


negone :: (Given Ctx) => Val
negone =  trace "negone" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_negone ctx


foreign import ccall "isl_val_one" c_one :: Ctx -> IO Val


one :: (Given Ctx) => Val
one =  trace "one" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_one ctx


foreign import ccall "isl_val_pow2" c_pow2 :: Val -> IO Val


pow2 :: (Given Ctx) => Val -> Val
pow2 = \v' -> trace "pow2" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_pow2 v


foreign import ccall "isl_val_sub" c_sub :: Val -> Val -> IO Val


sub :: (Given Ctx) => Val -> Val -> Val
sub = \v1' v2' -> trace "sub" $ 
    unsafePerformIO $ (return) =<< do
      v1 <- (return) v1'
      v2 <- (return) v2'

      let ctx = given :: Ctx
      c_sub v1 v2


foreign import ccall "isl_val_trunc" c_trunc :: Val -> IO Val


trunc :: (Given Ctx) => Val -> Val
trunc = \v' -> trace "trunc" $ 
    unsafePerformIO $ (return) =<< do
      v <- (return) v'

      let ctx = given :: Ctx
      c_trunc v


foreign import ccall "isl_val_zero" c_zero :: Ctx -> IO Val


zero :: (Given Ctx) => Val
zero =  trace "zero" $ 
    unsafePerformIO $ (return) =<< do

      let ctx = given :: Ctx
      c_zero ctx


foreign import ccall "isl_val_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Val


readFromStr :: (Given Ctx) => String -> Val
readFromStr = \str' -> trace "readFromStr" $ 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


