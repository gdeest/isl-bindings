{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.LocalSpace.Generated where

import Isl.Types
import Isl.Monad
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_local_space_dim" c_dim :: LocalSpaceRef -> DimType -> IO C.CInt

dim :: LocalSpaceRef -> DimType -> Int
dim ls typ =
    unsafePerformIO $ fromIntegral <$> c_dim ls typ


foreign import ccall "isl_local_space_find_dim_by_name" c_findDimByName :: LocalSpaceRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: LocalSpaceRef -> DimType -> String -> Int
findDimByName ls typ name =
    unsafePerformIO $ do
      name_c <- C.newCString name
      fromIntegral <$> c_findDimByName ls typ name_c


foreign import ccall "isl_local_space_dump" c_dump :: LocalSpaceRef -> IO ()

dump :: LocalSpaceRef -> ()
dump ls =
    unsafePerformIO $ c_dump ls


foreign import ccall "isl_local_space_get_dim_name" c_getDimName :: LocalSpaceRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: LocalSpaceRef -> DimType -> Int -> String
getDimName ls typ pos =
    unsafePerformIO $ C.peekCString =<< c_getDimName ls typ (fromIntegral pos)


foreign import ccall "isl_local_space_has_dim_id" c_hasDimId :: LocalSpaceRef -> DimType -> C.CUInt -> IO C.CBool

hasDimId :: LocalSpaceRef -> DimType -> Int -> Bool
hasDimId ls typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimId ls typ (fromIntegral pos)


foreign import ccall "isl_local_space_has_dim_name" c_hasDimName :: LocalSpaceRef -> DimType -> C.CUInt -> IO C.CBool

hasDimName :: LocalSpaceRef -> DimType -> Int -> Bool
hasDimName ls typ pos =
    unsafePerformIO $ M.toBool <$> c_hasDimName ls typ (fromIntegral pos)


foreign import ccall "isl_local_space_is_equal" c_isEqual :: LocalSpaceRef -> LocalSpaceRef -> IO C.CBool

isEqual :: LocalSpaceRef -> LocalSpaceRef -> Bool
isEqual ls1 ls2 =
    unsafePerformIO $ M.toBool <$> c_isEqual ls1 ls2


foreign import ccall "isl_local_space_is_params" c_isParams :: LocalSpaceRef -> IO C.CBool

isParams :: LocalSpaceRef -> Bool
isParams ls =
    unsafePerformIO $ M.toBool <$> c_isParams ls


foreign import ccall "isl_local_space_is_set" c_isSet :: LocalSpaceRef -> IO C.CBool

isSet :: LocalSpaceRef -> Bool
isSet ls =
    unsafePerformIO $ M.toBool <$> c_isSet ls


foreign import ccall "isl_local_space_get_space" c_getSpace :: LocalSpaceRef -> IO Space

getSpace :: MonadIO m => LocalSpaceRef -> IslT m Space
getSpace ls =
    unsafeIslFromIO $ \_ -> c_getSpace ls


foreign import ccall "isl_local_space_lifting" c_lifting :: LocalSpace -> IO BasicMap

lifting :: forall m. MonadIO m => LocalSpace %1 -> IslT m BasicMap
lifting = unsafeCoerce go where
  go :: LocalSpace -> IslT m BasicMap
  go ls =
    unsafeIslFromIO $ \_ -> c_lifting ls


foreign import ccall "isl_local_space_get_div" c_getDiv :: LocalSpaceRef -> C.CInt -> IO Aff

getDiv :: MonadIO m => LocalSpaceRef -> Int -> IslT m Aff
getDiv ls pos =
    unsafeIslFromIO $ \_ -> c_getDiv ls (fromIntegral pos)


foreign import ccall "isl_local_space_get_dim_id" c_getDimId :: LocalSpaceRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => LocalSpaceRef -> DimType -> Int -> IslT m Id
getDimId ls typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId ls typ (fromIntegral pos)


foreign import ccall "isl_local_space_add_dims" c_addDims :: LocalSpace -> DimType -> C.CUInt -> IO LocalSpace

addDims :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Int -> IslT m LocalSpace
addDims = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Int -> IslT m LocalSpace
  go ls typ n =
    unsafeIslFromIO $ \_ -> c_addDims ls typ (fromIntegral n)


foreign import ccall "isl_local_space_domain" c_domain :: LocalSpace -> IO LocalSpace

domain :: forall m. MonadIO m => LocalSpace %1 -> IslT m LocalSpace
domain = unsafeCoerce go where
  go :: LocalSpace -> IslT m LocalSpace
  go ls =
    unsafeIslFromIO $ \_ -> c_domain ls


foreign import ccall "isl_local_space_drop_dims" c_dropDims :: LocalSpace -> DimType -> C.CUInt -> C.CUInt -> IO LocalSpace

dropDims :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Int -> Int -> IslT m LocalSpace
dropDims = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Int -> Int -> IslT m LocalSpace
  go ls typ first n =
    unsafeIslFromIO $ \_ -> c_dropDims ls typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_local_space_flatten_domain" c_flattenDomain :: LocalSpace -> IO LocalSpace

flattenDomain :: forall m. MonadIO m => LocalSpace %1 -> IslT m LocalSpace
flattenDomain = unsafeCoerce go where
  go :: LocalSpace -> IslT m LocalSpace
  go ls =
    unsafeIslFromIO $ \_ -> c_flattenDomain ls


foreign import ccall "isl_local_space_flatten_range" c_flattenRange :: LocalSpace -> IO LocalSpace

flattenRange :: forall m. MonadIO m => LocalSpace %1 -> IslT m LocalSpace
flattenRange = unsafeCoerce go where
  go :: LocalSpace -> IslT m LocalSpace
  go ls =
    unsafeIslFromIO $ \_ -> c_flattenRange ls


foreign import ccall "isl_local_space_from_domain" c_fromDomain :: LocalSpace -> IO LocalSpace

fromDomain :: forall m. MonadIO m => LocalSpace %1 -> IslT m LocalSpace
fromDomain = unsafeCoerce go where
  go :: LocalSpace -> IslT m LocalSpace
  go ls =
    unsafeIslFromIO $ \_ -> c_fromDomain ls


foreign import ccall "isl_local_space_from_space" c_fromSpace :: Space -> IO LocalSpace

fromSpace :: forall m. MonadIO m => Space %1 -> IslT m LocalSpace
fromSpace = unsafeCoerce go where
  go :: Space -> IslT m LocalSpace
  go space =
    unsafeIslFromIO $ \_ -> c_fromSpace space


foreign import ccall "isl_local_space_insert_dims" c_insertDims :: LocalSpace -> DimType -> C.CUInt -> C.CUInt -> IO LocalSpace

insertDims :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Int -> Int -> IslT m LocalSpace
insertDims = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Int -> Int -> IslT m LocalSpace
  go ls typ first n =
    unsafeIslFromIO $ \_ -> c_insertDims ls typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_local_space_intersect" c_intersect :: LocalSpace -> LocalSpace -> IO LocalSpace

intersect :: forall m. MonadIO m => LocalSpace %1 -> LocalSpace %1 -> IslT m LocalSpace
intersect = unsafeCoerce go where
  go :: LocalSpace -> LocalSpace -> IslT m LocalSpace
  go ls1 ls2 =
    unsafeIslFromIO $ \_ -> c_intersect ls1 ls2


foreign import ccall "isl_local_space_range" c_range :: LocalSpace -> IO LocalSpace

range :: forall m. MonadIO m => LocalSpace %1 -> IslT m LocalSpace
range = unsafeCoerce go where
  go :: LocalSpace -> IslT m LocalSpace
  go ls =
    unsafeIslFromIO $ \_ -> c_range ls


foreign import ccall "isl_local_space_set_dim_id" c_setDimId :: LocalSpace -> DimType -> C.CUInt -> Id -> IO LocalSpace

setDimId :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Int -> Id %1 -> IslT m LocalSpace
setDimId = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Int -> Id -> IslT m LocalSpace
  go ls typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId ls typ (fromIntegral pos) id


foreign import ccall "isl_local_space_set_dim_name" c_setDimName :: LocalSpace -> DimType -> C.CUInt -> C.CString -> IO LocalSpace

setDimName :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Int -> String -> IslT m LocalSpace
setDimName = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Int -> String -> IslT m LocalSpace
  go ls typ pos s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setDimName ls typ (fromIntegral pos) s_c


foreign import ccall "isl_local_space_set_from_params" c_setFromParams :: LocalSpace -> IO LocalSpace

setFromParams :: forall m. MonadIO m => LocalSpace %1 -> IslT m LocalSpace
setFromParams = unsafeCoerce go where
  go :: LocalSpace -> IslT m LocalSpace
  go ls =
    unsafeIslFromIO $ \_ -> c_setFromParams ls


foreign import ccall "isl_local_space_set_tuple_id" c_setTupleId :: LocalSpace -> DimType -> Id -> IO LocalSpace

setTupleId :: forall m. MonadIO m => LocalSpace %1 -> DimType -> Id %1 -> IslT m LocalSpace
setTupleId = unsafeCoerce go where
  go :: LocalSpace -> DimType -> Id -> IslT m LocalSpace
  go ls typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId ls typ id


foreign import ccall "isl_local_space_wrap" c_wrap :: LocalSpace -> IO LocalSpace

wrap :: forall m. MonadIO m => LocalSpace %1 -> IslT m LocalSpace
wrap = unsafeCoerce go where
  go :: LocalSpace -> IslT m LocalSpace
  go ls =
    unsafeIslFromIO $ \_ -> c_wrap ls


foreign import ccall "isl_local_space_free" c_free :: LocalSpace -> IO ()

instance Consumable LocalSpace where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_local_space_copy" c_copy :: LocalSpace -> IO LocalSpace

instance Dupable LocalSpace where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow LocalSpace LocalSpaceRef where
  borrow = unsafeCoerce $ \(LocalSpace ptr) f -> let !r = f (LocalSpaceRef ptr) in (r, LocalSpace ptr)


