{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module Isl.UnionMap
  ( module Isl.UnionMap.Generated
  , foreachMap
  ) where

import Control.DeepSeq (NFData)
import Foreign.Ptr
import Foreign.C.Types
import Isl.Types.Raw
import Isl.UnionMap.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkMapCb)
import Isl.Map.Generated ()  -- Consumable/Borrow Map instances
import Isl.Monad.Internal (Isl, Ur)

foreign import ccall "isl_union_map_foreach_map"
  c_union_map_foreach_map :: UnionMap -> FunPtr (RawCallback Map) -> Ptr () -> IO CInt

-- | Iterate over Maps of a UnionMap. The UnionMap is borrowed.
-- Each Map is auto-freed after the callback returns.
foreachMap
  :: NFData r
  => UnionMapRef s
  -> (forall s'. MapRef s' -> Isl (Ur r))
  -> Isl (Ur [r])
foreachMap (UnionMapRef umPtr) f =
  foreachCollect mkMapCb
    (\cb -> c_union_map_foreach_map (UnionMap umPtr) cb nullPtr) f
