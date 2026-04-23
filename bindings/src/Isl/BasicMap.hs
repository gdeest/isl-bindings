{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module Isl.BasicMap
  ( module Isl.BasicMap.Generated
  , foreachConstraint
  ) where

import Control.DeepSeq (NFData)
import Foreign.Ptr
import Foreign.C.Types
import Isl.Types.Raw
import Isl.BasicMap.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkConstraintCb)
import Isl.Constraint.Generated ()  -- Consumable/Borrow Constraint instances
import Isl.Monad.Internal (Isl, Ur)

foreign import ccall "isl_basic_map_foreach_constraint"
  c_basic_map_foreach_constraint :: BasicMap -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicMap. The BasicMap is borrowed.
-- Each constraint is auto-freed after the callback returns.
foreachConstraint
  :: NFData r
  => BasicMapRef s
  -> (forall s'. ConstraintRef s' -> Isl (Ur r))
  -> Isl (Ur [r])
foreachConstraint (BasicMapRef bmPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_map_foreach_constraint (BasicMap bmPtr) cb nullPtr) f
