{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module Isl.Set
  ( module Isl.Set.Generated
  , foreachBasicSet
  ) where

import Control.DeepSeq (NFData)
import Foreign.Ptr
import Foreign.C.Types
import Isl.Types.Raw
import Isl.Set.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkBasicSetCb)
import Isl.BasicSet.Generated ()  -- Consumable/Borrow BasicSet instances
import Isl.Monad.Internal (Isl, Ur)

foreign import ccall "isl_set_foreach_basic_set"
  c_set_foreach_basic_set :: Set -> FunPtr (RawCallback BasicSet) -> Ptr () -> IO CInt

-- | Iterate over BasicSets of a Set. The Set is borrowed (__isl_keep).
-- Each BasicSet is auto-freed after the callback returns.
foreachBasicSet
  :: NFData r
  => SetRef s
  -> (forall s'. BasicSetRef s' -> Isl (Ur r))
  -> Isl (Ur [r])
foreachBasicSet (SetRef sPtr) f =
  foreachCollect mkBasicSetCb
    (\cb -> c_set_foreach_basic_set (Set sPtr) cb nullPtr) f
