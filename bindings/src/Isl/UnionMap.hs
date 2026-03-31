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

foreign import ccall "isl_union_map_foreach_map"
  c_union_map_foreach_map :: UnionMap -> FunPtr (RawCallback Map) -> Ptr () -> IO CInt

-- | Iterate over Maps of a UnionMap. The UnionMap is borrowed.
-- Each Map is __isl_take.
foreachMap :: UnionMapRef -> (Map -> IO a) -> IO [a]
foreachMap (UnionMapRef umPtr) f =
  foreachCollect mkMapCb
    (\cb -> c_union_map_foreach_map (UnionMap umPtr) cb nullPtr) f
