{-# LANGUAGE ForeignFunctionInterface #-}

module Isl.UnionMap
  ( module Isl.UnionMap.Generated
  , foreachMap
  ) where

import Foreign.Ptr
import Foreign.C.Types
import Isl.Types
import Isl.UnionMap.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkMapCb)
import Isl.Map.Generated ()  -- Consumable/Borrow Map instances

foreign import ccall "isl_union_map_foreach_map"
  c_union_map_foreach_map :: UnionMap -> FunPtr (RawCallback Map) -> Ptr () -> IO CInt

-- | Iterate over Maps of a UnionMap. The UnionMap is borrowed.
-- Each Map is auto-freed after the callback returns.
foreachMap :: UnionMapRef -> (MapRef -> IO a) -> IO [a]
foreachMap (UnionMapRef umPtr) f =
  foreachCollect mkMapCb
    (\cb -> c_union_map_foreach_map (UnionMap umPtr) cb nullPtr) f
