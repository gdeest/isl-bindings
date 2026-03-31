{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.MultiAff
  ( module Isl.MultiAff.Generated
    -- * Composite extraction
  , multiAffGetAffCopy
  ) where

import Foreign.C.Types
import Isl.Types
import Isl.MultiAff.Generated

-- | Get a copy of the aff for output dimension @pos@.
-- The returned Aff is a fresh copy (caller must free).
foreign import ccall "isl_multi_aff_get_aff"
  multiAffGetAffCopy :: MultiAffRef -> CInt -> IO Aff
