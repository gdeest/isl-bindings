{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module Isl.UnionSet
  ( module Isl.UnionSet.Generated
  , foreachSet
  ) where

import Control.DeepSeq (NFData)
import Foreign.Ptr
import Foreign.C.Types
import Isl.Types.Raw
import Isl.UnionSet.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkSetCb)
import Isl.Set.Generated ()  -- Consumable/Borrow Set instances
import Isl.Monad.Internal (Isl, Ur)

foreign import ccall "isl_union_set_foreach_set"
  c_union_set_foreach_set :: UnionSet -> FunPtr (RawCallback Set) -> Ptr () -> IO CInt

-- | Iterate over Sets of a UnionSet. The UnionSet is borrowed (__isl_keep).
-- Each Set is auto-freed after the callback returns.
foreachSet
  :: NFData r
  => UnionSetRef s
  -> (forall s'. SetRef s' -> Isl (Ur r))
  -> Isl (Ur [r])
foreachSet (UnionSetRef usPtr) f =
  foreachCollect mkSetCb
    (\cb -> c_union_set_foreach_set (UnionSet usPtr) cb nullPtr) f
