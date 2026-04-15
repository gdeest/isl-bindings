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
import Isl.Constraint.Generated ()  -- Consumable/Borrow Constraint instances

foreign import ccall "isl_basic_set_foreach_constraint"
  c_basic_set_foreach_constraint :: BasicSet -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicSet. The BasicSet is borrowed
-- (__isl_keep). Each constraint is auto-freed after the callback returns.
foreachConstraint :: BasicSetRef -> (ConstraintRef -> IO a) -> IO [a]
foreachConstraint (BasicSetRef bsPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_set_foreach_constraint (BasicSet bsPtr) cb nullPtr) f
