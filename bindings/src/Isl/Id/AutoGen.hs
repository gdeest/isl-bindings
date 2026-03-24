{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.Id.AutoGen where

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

foreign import ccall "isl_id_get_ctx" c_getCtx :: Id -> IO Ctx


getCtx :: (Given Ctx) => Id -> Ctx
getCtx = \id' -> trace "getCtx" $ 
    unsafePerformIO $ (return) =<< do
      id <- (return) id'

      let ctx = given :: Ctx
      c_getCtx id


foreign import ccall "isl_id_dump" c_dump :: Id -> IO ()


dump :: (Given Ctx) => Id -> ()
dump = \id' -> trace "dump" $ 
    unsafePerformIO $ (return) =<< do
      id <- (return) id'

      let ctx = given :: Ctx
      c_dump id


foreign import ccall "isl_id_copy" c_copy :: Id -> IO Id


copy :: (Given Ctx) => Id -> Id
copy = \id' -> trace "copy" $ 
    unsafePerformIO $ (return) =<< do
      id <- (return) id'

      let ctx = given :: Ctx
      c_copy id


foreign import ccall "isl_id_to_str" c_toStr :: Id -> IO C.CString


toStr :: (Given Ctx) => Id -> String
toStr = \id' -> trace "toStr" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      id <- (return) id'

      let ctx = given :: Ctx
      c_toStr id


foreign import ccall "isl_id_get_name" c_getName :: Id -> IO C.CString


getName :: (Given Ctx) => Id -> String
getName = \id' -> trace "getName" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      id <- (return) id'

      let ctx = given :: Ctx
      c_getName id


foreign import ccall "isl_id_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Id


readFromStr :: (Given Ctx) => String -> Id
readFromStr = \str' -> trace "readFromStr" $ 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


