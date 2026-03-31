{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.BasicSet
  ( module Isl.BasicSet.Generated
  , foreachConstraint
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Isl.Types
import Isl.BasicSet.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkConstraintCb)

foreign import ccall "isl_basic_set_foreach_constraint"
  c_basic_set_foreach_constraint :: BasicSet -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicSet. The BasicSet is borrowed
-- (__isl_keep). Each Constraint passed to the callback is __isl_take.
foreachConstraint :: BasicSetRef -> (Constraint -> IO a) -> IO [a]
foreachConstraint (BasicSetRef bsPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_set_foreach_constraint (BasicSet bsPtr) cb nullPtr) f
