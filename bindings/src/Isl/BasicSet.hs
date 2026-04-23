{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module Isl.BasicSet
  ( module Isl.BasicSet.Generated
  , foreachConstraint
  ) where

import Control.DeepSeq (NFData)
import Foreign.Ptr
import Foreign.C.Types
import Isl.Types.Raw
import Isl.BasicSet.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkConstraintCb)
import Isl.Constraint.Generated ()  -- Consumable/Borrow Constraint instances
import Isl.Monad.Internal (Isl, Ur)

foreign import ccall "isl_basic_set_foreach_constraint"
  c_basic_set_foreach_constraint :: BasicSet -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicSet. The BasicSet is borrowed
-- (__isl_keep). Each constraint is auto-freed after the callback returns.
foreachConstraint
  :: NFData r
  => BasicSetRef s
  -> (forall s'. ConstraintRef s' -> Isl (Ur r))
  -> Isl (Ur [r])
foreachConstraint (BasicSetRef bsPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_set_foreach_constraint (BasicSet bsPtr) cb nullPtr) f
