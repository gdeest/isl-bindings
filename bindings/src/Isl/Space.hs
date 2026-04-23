{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.Space
  ( module Isl.Space.Generated
    -- * Name queries with Maybe (ref-based)
  , spaceGetTupleNameRef
  , spaceGetDimNameRef
  ) where

import Foreign.C.Types
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import Isl.Types (DimType(..))
import Isl.Types.Raw
import Isl.Space.Generated

foreign import ccall "isl_space_has_tuple_name"
  c_space_has_tuple_name :: SpaceRef s -> CInt -> IO CInt

foreign import ccall "isl_space_get_tuple_name"
  c_space_get_tuple_name :: SpaceRef s -> CInt -> IO CString

foreign import ccall "isl_space_get_dim_name"
  c_space_get_dim_name :: SpaceRef s -> CInt -> CInt -> IO CString

-- | Get the tuple name for a given dimension type (e.g. set tuple names
-- like @S0@ in @{ S0[i,j] : ... }@).
-- Returns 'Nothing' if no tuple name is set.  Pure since the Space is
-- only borrowed (__isl_keep) — evaluated strictly at the call site.
spaceGetTupleNameRef :: SpaceRef s -> DimType -> Maybe String
spaceGetTupleNameRef sp (DimType dt) =
  let !r = unsafePerformIO $ do
        has <- c_space_has_tuple_name sp dt
        if has /= 0
          then Just <$> (c_space_get_tuple_name sp dt >>= peekCString)
          else return Nothing
  in r

-- | Get the name of a specific dimension (e.g. parameter names).
-- Returns 'Nothing' if the dimension has no name or the pointer is null.
spaceGetDimNameRef :: SpaceRef s -> DimType -> Int -> Maybe String
spaceGetDimNameRef sp (DimType dt) pos =
  let !r = unsafePerformIO $ do
        ptr <- c_space_get_dim_name sp dt (fromIntegral pos)
        if ptr == nullPtr then return Nothing
        else Just <$> peekCString ptr
  in r
