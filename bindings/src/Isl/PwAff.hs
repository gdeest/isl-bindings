{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.PwAff
  ( module Isl.PwAff.Generated
    -- * Piece iteration
  , foreachPiece
  ) where

import Foreign.C.Types
import Foreign.Ptr
import Isl.Types
import Isl.PwAff.Generated
import Isl.Foreach (RawCallback2, mkPwAffPieceCb, foreachCollect2)
import Isl.Set.Generated ()  -- Consumable/Borrow Set instances
import Isl.Aff.Generated ()  -- Consumable/Borrow Aff instances

foreign import ccall "isl_pw_aff_foreach_piece"
  c_foreachPiece :: PwAffRef -> FunPtr (RawCallback2 Set Aff) -> Ptr () -> IO CInt

-- | Iterate over the pieces of a PwAff, collecting results.
-- Each piece gives a borrowed SetRef (domain) and AffRef (expression).
-- Both are auto-freed after the callback returns.
foreachPiece :: PwAffRef -> (SetRef -> AffRef -> IO r) -> IO [r]
foreachPiece pa process =
  foreachCollect2 mkPwAffPieceCb (\cb -> c_foreachPiece pa cb nullPtr) process
