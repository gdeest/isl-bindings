{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.UnionSet
  ( module Isl.UnionSet.Generated
  , foreachSet
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Isl.Types
import Isl.UnionSet.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkSetCb)

foreign import ccall "isl_union_set_foreach_set"
  c_union_set_foreach_set :: UnionSet -> FunPtr (RawCallback Set) -> Ptr () -> IO CInt

-- | Iterate over Sets of a UnionSet. The UnionSet is borrowed (__isl_keep).
-- Each Set passed to the callback is __isl_take.
foreachSet :: UnionSetRef -> (Set -> IO a) -> IO [a]
foreachSet (UnionSetRef usPtr) f =
  foreachCollect mkSetCb
    (\cb -> c_union_set_foreach_set (UnionSet usPtr) cb nullPtr) f
