{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.AffList.Generated where

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

foreign import ccall "isl_aff_list_n_aff" c_nAff :: AffListRef s_list -> IO C.CInt

nAff :: AffListRef s_list -> Int
nAff list =
    let !r = unsafePerformIO $ fromIntegral <$> c_nAff list in r


foreign import ccall "isl_aff_list_dump" c_dump :: AffListRef s_list -> IO ()

dump :: AffListRef s_list -> ()
dump list =
    let !r = unsafePerformIO $ c_dump list in r


foreign import ccall "isl_aff_list_to_str" c_toStr :: AffListRef s_list -> IO C.CString

toStr :: AffListRef s_list -> String
toStr list =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr list in r


foreign import ccall "isl_aff_list_reverse" c_reverse :: AffList -> IO AffList

reverse :: forall m s_list. MonadIO m => AffList %1 -> IslT m AffList
reverse = unsafeCoerce go where
  go :: AffList -> IslT m AffList
  go list =
    unsafeIslFromIO $ \_ -> c_reverse list


foreign import ccall "isl_aff_list_swap" c_swap :: AffList -> C.CUInt -> C.CUInt -> IO AffList

swap :: forall m s_list. MonadIO m => AffList %1 -> Int -> Int -> IslT m AffList
swap = unsafeCoerce go where
  go :: AffList -> Int -> Int -> IslT m AffList
  go list pos1 pos2 =
    unsafeIslFromIO $ \_ -> c_swap list (fromIntegral pos1) (fromIntegral pos2)


foreign import ccall "isl_aff_list_size" c_size :: AffListRef s_list -> IO C.CInt

size :: AffListRef s_list -> Int
size list =
    let !r = unsafePerformIO $ fromIntegral <$> c_size list in r


foreign import ccall "isl_aff_list_get_at" c_getAt :: AffListRef s_list -> C.CInt -> IO Aff

getAt :: MonadIO m => AffListRef s_list -> Int -> IslT m Aff
getAt list index =
    unsafeIslFromIO $ \_ -> c_getAt list (fromIntegral index)


foreign import ccall "isl_aff_list_add" c_add :: AffList -> Aff -> IO AffList

add :: forall m s_list s_el. MonadIO m => AffList %1 -> Aff %1 -> IslT m AffList
add = unsafeCoerce go where
  go :: AffList -> Aff -> IslT m AffList
  go list el =
    unsafeIslFromIO $ \_ -> c_add list el


foreign import ccall "isl_aff_list_clear" c_clear :: AffList -> IO AffList

clear :: forall m s_list. MonadIO m => AffList %1 -> IslT m AffList
clear = unsafeCoerce go where
  go :: AffList -> IslT m AffList
  go list =
    unsafeIslFromIO $ \_ -> c_clear list


foreign import ccall "isl_aff_list_concat" c_concat :: AffList -> AffList -> IO AffList

concat :: forall m s_list1 s_list2. MonadIO m => AffList %1 -> AffList %1 -> IslT m AffList
concat = unsafeCoerce go where
  go :: AffList -> AffList -> IslT m AffList
  go list1 list2 =
    unsafeIslFromIO $ \_ -> c_concat list1 list2


foreign import ccall "isl_aff_list_drop" c_drop :: AffList -> C.CUInt -> C.CUInt -> IO AffList

drop :: forall m s_list. MonadIO m => AffList %1 -> Int -> Int -> IslT m AffList
drop = unsafeCoerce go where
  go :: AffList -> Int -> Int -> IslT m AffList
  go list first n =
    unsafeIslFromIO $ \_ -> c_drop list (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_aff_list_insert" c_insert :: AffList -> C.CUInt -> Aff -> IO AffList

insert :: forall m s_list s_el. MonadIO m => AffList %1 -> Int -> Aff %1 -> IslT m AffList
insert = unsafeCoerce go where
  go :: AffList -> Int -> Aff -> IslT m AffList
  go list pos el =
    unsafeIslFromIO $ \_ -> c_insert list (fromIntegral pos) el


foreign import ccall "isl_aff_list_set_at" c_setAt :: AffList -> C.CInt -> Aff -> IO AffList

setAt :: forall m s_list s_el. MonadIO m => AffList %1 -> Int -> Aff %1 -> IslT m AffList
setAt = unsafeCoerce go where
  go :: AffList -> Int -> Aff -> IslT m AffList
  go list index el =
    unsafeIslFromIO $ \_ -> c_setAt list (fromIntegral index) el


foreign import ccall "isl_aff_list_alloc" c_alloc :: Ctx -> C.CInt -> IO AffList

alloc :: MonadIO m => Int -> IslT m AffList
alloc n =
    unsafeIslFromIO $ \ctx -> c_alloc ctx (fromIntegral n)


foreign import ccall "isl_aff_list_from_aff" c_fromAff :: Aff -> IO AffList

fromAff :: forall m s_el. MonadIO m => Aff %1 -> IslT m AffList
fromAff = unsafeCoerce go where
  go :: Aff -> IslT m AffList
  go el =
    unsafeIslFromIO $ \_ -> c_fromAff el


foreign import ccall "isl_aff_list_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO AffList

readFromStr :: MonadIO m => String -> IslT m AffList
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_aff_list_free" c_free :: AffList -> IO ()

instance Consumable AffList where
  consume = unsafeCoerce c_free


foreign import ccall "isl_aff_list_copy" c_copy :: AffList -> IO AffList

instance Dupable AffList where
  dup = unsafeCoerce $ \x -> do
    copy <- c_copy x
    return (x, copy)


instance Borrow AffList AffListRef where
  borrow = unsafeCoerce $ \(AffList ptr) f -> let !r = f (AffListRef ptr) in (r, AffList ptr)


