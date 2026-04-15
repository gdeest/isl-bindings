{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.BasicMap
  ( module Isl.BasicMap.Generated
  , foreachConstraint
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Isl.Types
import Isl.BasicMap.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkConstraintCb)
import Isl.Constraint.Generated ()  -- Consumable/Borrow Constraint instances

foreign import ccall "isl_basic_map_foreach_constraint"
  c_basic_map_foreach_constraint :: BasicMap -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicMap. The BasicMap is borrowed.
-- Each constraint is auto-freed after the callback returns.
foreachConstraint :: BasicMapRef -> (ConstraintRef -> IO a) -> IO [a]
foreachConstraint (BasicMapRef bmPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_map_foreach_constraint (BasicMap bmPtr) cb nullPtr) f
