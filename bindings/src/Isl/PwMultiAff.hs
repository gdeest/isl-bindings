{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.PwMultiAff
  ( module Isl.PwMultiAff.Generated
    -- * Piece iteration
  , foreachPiece
  ) where

import Foreign.C.Types
import Foreign.Ptr
import Isl.Types
import Isl.PwMultiAff.Generated
import Isl.Foreach (RawCallback2, mkPwMultiAffPieceCb, foreachCollect2)

foreign import ccall "isl_pw_multi_aff_foreach_piece"
  c_foreachPiece :: PwMultiAffRef -> FunPtr (RawCallback2 Set MultiAff) -> Ptr () -> IO CInt

-- | Iterate over the pieces of a PwMultiAff, collecting results.
-- Each piece gives an __isl_take Set (domain) and __isl_take MultiAff (expression).
foreachPiece :: PwMultiAffRef -> (Set -> MultiAff -> IO r) -> IO [r]
foreachPiece pma process =
  foreachCollect2 mkPwMultiAffPieceCb (\cb -> c_foreachPiece pma cb nullPtr) process
