{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.LocalSpace.AutoGen where

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

foreign import ccall "isl_local_space_find_dim_by_name" c_findDimByName :: LocalSpace -> DimType -> C.CString -> IO C.CInt


findDimByName :: (Given Ctx) => LocalSpace -> DimType -> String -> Int
findDimByName = \ls' typ' name' -> trace "findDimByName" $ 
    unsafePerformIO $ (return . fromIntegral) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_findDimByName ls typ name


foreign import ccall "isl_local_space_get_ctx" c_getCtx :: LocalSpace -> IO Ctx


getCtx :: (Given Ctx) => LocalSpace -> Ctx
getCtx = \ls' -> trace "getCtx" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_getCtx ls


foreign import ccall "isl_local_space_dump" c_dump :: LocalSpace -> IO ()


dump :: (Given Ctx) => LocalSpace -> ()
dump = \ls' -> trace "dump" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_dump ls


foreign import ccall "isl_local_space_get_dim_name" c_getDimName :: LocalSpace -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => LocalSpace -> DimType -> Int -> String
getDimName = \ls' typ' pos' -> trace "getDimName" $ 
    unsafePerformIO $ (C.peekCString) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName ls typ pos


foreign import ccall "isl_local_space_has_dim_id" c_hasDimId :: LocalSpace -> DimType -> C.CUInt -> IO C.CBool


hasDimId :: (Given Ctx) => LocalSpace -> DimType -> Int -> Bool
hasDimId = \ls' typ' pos' -> trace "hasDimId" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimId ls typ pos


foreign import ccall "isl_local_space_has_dim_name" c_hasDimName :: LocalSpace -> DimType -> C.CUInt -> IO C.CBool


hasDimName :: (Given Ctx) => LocalSpace -> DimType -> Int -> Bool
hasDimName = \ls' typ' pos' -> trace "hasDimName" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimName ls typ pos


foreign import ccall "isl_local_space_is_equal" c_isEqual :: LocalSpace -> LocalSpace -> IO C.CBool


isEqual :: (Given Ctx) => LocalSpace -> LocalSpace -> Bool
isEqual = \ls1' ls2' -> trace "isEqual" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      ls1 <- (return) ls1'
      ls2 <- (return) ls2'

      let ctx = given :: Ctx
      c_isEqual ls1 ls2


foreign import ccall "isl_local_space_is_params" c_isParams :: LocalSpace -> IO C.CBool


isParams :: (Given Ctx) => LocalSpace -> Bool
isParams = \ls' -> trace "isParams" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_isParams ls


foreign import ccall "isl_local_space_is_set" c_isSet :: LocalSpace -> IO C.CBool


isSet :: (Given Ctx) => LocalSpace -> Bool
isSet = \ls' -> trace "isSet" $ 
    unsafePerformIO $ (return . M.toBool) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_isSet ls


foreign import ccall "isl_local_space_get_space" c_getSpace :: LocalSpace -> IO Space


getSpace :: (Given Ctx) => LocalSpace -> Space
getSpace = \ls' -> trace "getSpace" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_getSpace ls


foreign import ccall "isl_local_space_lifting" c_lifting :: LocalSpace -> IO BasicMap


lifting :: (Given Ctx) => LocalSpace -> BasicMap
lifting = \ls' -> trace "lifting" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_lifting ls


foreign import ccall "isl_local_space_get_div" c_getDiv :: LocalSpace -> C.CInt -> IO Aff


getDiv :: (Given Ctx) => LocalSpace -> Int -> Aff
getDiv = \ls' pos' -> trace "getDiv" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDiv ls pos


foreign import ccall "isl_local_space_get_dim_id" c_getDimId :: LocalSpace -> DimType -> C.CUInt -> IO Id


getDimId :: (Given Ctx) => LocalSpace -> DimType -> Int -> Id
getDimId = \ls' typ' pos' -> trace "getDimId" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimId ls typ pos


foreign import ccall "isl_local_space_add_dims" c_addDims :: LocalSpace -> DimType -> C.CUInt -> IO LocalSpace


addDims :: (Given Ctx) => LocalSpace -> DimType -> Int -> LocalSpace
addDims = \ls' typ' n' -> trace "addDims" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_addDims ls typ n


foreign import ccall "isl_local_space_copy" c_copy :: LocalSpace -> IO LocalSpace


copy :: (Given Ctx) => LocalSpace -> LocalSpace
copy = \ls' -> trace "copy" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_copy ls


foreign import ccall "isl_local_space_domain" c_domain :: LocalSpace -> IO LocalSpace


domain :: (Given Ctx) => LocalSpace -> LocalSpace
domain = \ls' -> trace "domain" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_domain ls


foreign import ccall "isl_local_space_drop_dims" c_dropDims :: LocalSpace -> DimType -> C.CUInt -> C.CUInt -> IO LocalSpace


dropDims :: (Given Ctx) => LocalSpace -> DimType -> Int -> Int -> LocalSpace
dropDims = \ls' typ' first' n' -> trace "dropDims" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropDims ls typ first n


foreign import ccall "isl_local_space_flatten_domain" c_flattenDomain :: LocalSpace -> IO LocalSpace


flattenDomain :: (Given Ctx) => LocalSpace -> LocalSpace
flattenDomain = \ls' -> trace "flattenDomain" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_flattenDomain ls


foreign import ccall "isl_local_space_flatten_range" c_flattenRange :: LocalSpace -> IO LocalSpace


flattenRange :: (Given Ctx) => LocalSpace -> LocalSpace
flattenRange = \ls' -> trace "flattenRange" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_flattenRange ls


foreign import ccall "isl_local_space_from_domain" c_fromDomain :: LocalSpace -> IO LocalSpace


fromDomain :: (Given Ctx) => LocalSpace -> LocalSpace
fromDomain = \ls' -> trace "fromDomain" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_fromDomain ls


foreign import ccall "isl_local_space_from_space" c_fromSpace :: Space -> IO LocalSpace


fromSpace :: (Given Ctx) => Space -> LocalSpace
fromSpace = \space' -> trace "fromSpace" $ 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_fromSpace space


foreign import ccall "isl_local_space_insert_dims" c_insertDims :: LocalSpace -> DimType -> C.CUInt -> C.CUInt -> IO LocalSpace


insertDims :: (Given Ctx) => LocalSpace -> DimType -> Int -> Int -> LocalSpace
insertDims = \ls' typ' first' n' -> trace "insertDims" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_insertDims ls typ first n


foreign import ccall "isl_local_space_intersect" c_intersect :: LocalSpace -> LocalSpace -> IO LocalSpace


intersect :: (Given Ctx) => LocalSpace -> LocalSpace -> LocalSpace
intersect = \ls1' ls2' -> trace "intersect" $ 
    unsafePerformIO $ (return) =<< do
      ls1 <- (return) ls1'
      ls2 <- (return) ls2'

      let ctx = given :: Ctx
      c_intersect ls1 ls2


foreign import ccall "isl_local_space_range" c_range :: LocalSpace -> IO LocalSpace


range :: (Given Ctx) => LocalSpace -> LocalSpace
range = \ls' -> trace "range" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_range ls


foreign import ccall "isl_local_space_set_dim_id" c_setDimId :: LocalSpace -> DimType -> C.CUInt -> Id -> IO LocalSpace


setDimId :: (Given Ctx) => LocalSpace -> DimType -> Int -> Id -> LocalSpace
setDimId = \ls' typ' pos' id' -> trace "setDimId" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setDimId ls typ pos id


foreign import ccall "isl_local_space_set_dim_name" c_setDimName :: LocalSpace -> DimType -> C.CUInt -> C.CString -> IO LocalSpace


setDimName :: (Given Ctx) => LocalSpace -> DimType -> Int -> String -> LocalSpace
setDimName = \ls' typ' pos' s' -> trace "setDimName" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setDimName ls typ pos s


foreign import ccall "isl_local_space_set_from_params" c_setFromParams :: LocalSpace -> IO LocalSpace


setFromParams :: (Given Ctx) => LocalSpace -> LocalSpace
setFromParams = \ls' -> trace "setFromParams" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_setFromParams ls


foreign import ccall "isl_local_space_set_tuple_id" c_setTupleId :: LocalSpace -> DimType -> Id -> IO LocalSpace


setTupleId :: (Given Ctx) => LocalSpace -> DimType -> Id -> LocalSpace
setTupleId = \ls' typ' id' -> trace "setTupleId" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setTupleId ls typ id


foreign import ccall "isl_local_space_wrap" c_wrap :: LocalSpace -> IO LocalSpace


wrap :: (Given Ctx) => LocalSpace -> LocalSpace
wrap = \ls' -> trace "wrap" $ 
    unsafePerformIO $ (return) =<< do
      ls <- (return) ls'

      let ctx = given :: Ctx
      c_wrap ls


