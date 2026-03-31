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

foreign import ccall "isl_basic_map_foreach_constraint"
  c_basic_map_foreach_constraint :: BasicMap -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicMap. The BasicMap is borrowed.
-- Each Constraint is __isl_take.
foreachConstraint :: BasicMapRef -> (Constraint -> IO a) -> IO [a]
foreachConstraint (BasicMapRef bmPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_map_foreach_constraint (BasicMap bmPtr) cb nullPtr) f
