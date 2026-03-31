{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.Map
  ( module Isl.Map.Generated
  , foreachBasicMap
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Isl.Types
import Isl.Map.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkBasicMapCb)

foreign import ccall "isl_map_foreach_basic_map"
  c_map_foreach_basic_map :: Map -> FunPtr (RawCallback BasicMap) -> Ptr () -> IO CInt

-- | Iterate over BasicMaps of a Map. The Map is borrowed.
-- Each BasicMap is __isl_take.
foreachBasicMap :: MapRef -> (BasicMap -> IO a) -> IO [a]
foreachBasicMap (MapRef mPtr) f =
  foreachCollect mkBasicMapCb
    (\cb -> c_map_foreach_basic_map (Map mPtr) cb nullPtr) f
