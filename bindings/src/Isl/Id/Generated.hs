{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.Id.Generated where

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

foreign import ccall "isl_id_dump" c_dump :: IdRef -> IO ()

dump :: IdRef -> ()
dump id =
    let !r = unsafePerformIO $ c_dump id in r


foreign import ccall "isl_id_to_str" c_toStr :: IdRef -> IO C.CString

toStr :: IdRef -> String
toStr id =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr id in r


foreign import ccall "isl_id_get_name" c_getName :: IdRef -> IO C.CString

getName :: IdRef -> String
getName id =
    let !r = unsafePerformIO $ C.peekCString =<< c_getName id in r


foreign import ccall "isl_id_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO Id

readFromStr :: MonadIO m => String -> IslT m Id
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_id_free" c_free :: Id -> IO ()

instance Consumable Id where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_id_copy" c_copy :: Id -> IO Id

instance Dupable Id where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow Id IdRef where
  borrow = unsafeCoerce $ \(Id ptr) f -> let !r = f (IdRef ptr) in (r, Id ptr)


