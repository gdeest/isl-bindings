{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.Space
  ( module Isl.Space.Generated
    -- * Name queries with Maybe
  , spaceGetTupleName
  , spaceGetDimName
  ) where

import Foreign.C.Types
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (nullPtr)
import Isl.Types
import Isl.Space.Generated

foreign import ccall "isl_space_has_tuple_name"
  c_space_has_tuple_name :: Space -> CInt -> IO CInt

foreign import ccall "isl_space_get_tuple_name"
  c_space_get_tuple_name :: Space -> CInt -> IO CString

foreign import ccall "isl_space_get_dim_name"
  c_space_get_dim_name :: Space -> CInt -> CInt -> IO CString

-- | Get the tuple name for a given dimension type (e.g. set tuple names
-- like @S0@ in @{ S0[i,j] : ... }@).
-- Returns 'Nothing' if no tuple name is set.
-- The Space is __isl_keep.
spaceGetTupleName :: Space -> DimType -> IO (Maybe String)
spaceGetTupleName sp (DimType dt) = do
  has <- c_space_has_tuple_name sp dt
  if has /= 0
    then Just <$> (c_space_get_tuple_name sp dt >>= peekCString)
    else return Nothing

-- | Get the name of a specific dimension (e.g. parameter names).
-- Returns 'Nothing' if the dimension has no name or the pointer is null.
-- The Space is __isl_keep.
spaceGetDimName :: Space -> DimType -> Int -> IO (Maybe String)
spaceGetDimName sp (DimType dt) pos = do
  ptr <- c_space_get_dim_name sp dt (fromIntegral pos)
  if ptr == nullPtr then return Nothing
  else Just <$> peekCString ptr
