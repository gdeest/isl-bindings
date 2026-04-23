{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module Isl.PwMultiAff
  ( module Isl.PwMultiAff.Generated
    -- * Piece iteration
  , foreachPiece
  ) where

import Control.DeepSeq (NFData)
import Foreign.C.Types
import Foreign.Ptr
import Isl.Types.Raw
import Isl.PwMultiAff.Generated
import Isl.Foreach (RawCallback2, mkPwMultiAffPieceCb, foreachCollect2)
import Isl.Set.Generated ()  -- Consumable/Borrow Set instances
import Isl.MultiAff.Generated ()  -- Consumable/Borrow MultiAff instances
import Isl.Monad.Internal (Isl, Ur)

foreign import ccall "isl_pw_multi_aff_foreach_piece"
  c_foreachPiece :: PwMultiAffRef s -> FunPtr (RawCallback2 Set MultiAff) -> Ptr () -> IO CInt

-- | Iterate over the pieces of a PwMultiAff, collecting results.
-- Each piece gives a borrowed SetRef (domain) and MultiAffRef (expression).
-- Both are auto-freed after the callback returns.
foreachPiece
  :: NFData r
  => PwMultiAffRef s
  -> (forall s'. SetRef s' -> MultiAffRef s' -> Isl (Ur r))
  -> Isl (Ur [r])
foreachPiece pma process =
  foreachCollect2 mkPwMultiAffPieceCb (\cb -> c_foreachPiece pma cb nullPtr) process
