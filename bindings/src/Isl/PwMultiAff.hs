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
import Isl.Set.Generated ()  -- Consumable/Borrow Set instances
import Isl.MultiAff.Generated ()  -- Consumable/Borrow MultiAff instances

foreign import ccall "isl_pw_multi_aff_foreach_piece"
  c_foreachPiece :: PwMultiAffRef -> FunPtr (RawCallback2 Set MultiAff) -> Ptr () -> IO CInt

-- | Iterate over the pieces of a PwMultiAff, collecting results.
-- Each piece gives a borrowed SetRef (domain) and MultiAffRef (expression).
-- Both are auto-freed after the callback returns.
foreachPiece :: PwMultiAffRef -> (SetRef -> MultiAffRef -> IO r) -> IO [r]
foreachPiece pma process =
  foreachCollect2 mkPwMultiAffPieceCb (\cb -> c_foreachPiece pma cb nullPtr) process
