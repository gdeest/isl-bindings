{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.Set
  ( module Isl.Set.Generated
  , foreachBasicSet
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Isl.Types
import Isl.Set.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkBasicSetCb)
import Isl.BasicSet.Generated ()  -- Consumable/Borrow BasicSet instances

foreign import ccall "isl_set_foreach_basic_set"
  c_set_foreach_basic_set :: Set -> FunPtr (RawCallback BasicSet) -> Ptr () -> IO CInt

-- | Iterate over BasicSets of a Set. The Set is borrowed (__isl_keep).
-- Each BasicSet is auto-freed after the callback returns.
foreachBasicSet :: SetRef -> (BasicSetRef -> IO a) -> IO [a]
foreachBasicSet (SetRef sPtr) f =
  foreachCollect mkBasicSetCb
    (\cb -> c_set_foreach_basic_set (Set sPtr) cb nullPtr) f
