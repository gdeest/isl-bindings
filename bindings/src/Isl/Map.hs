{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module Isl.Map
  ( module Isl.Map.Generated
  , foreachBasicMap
  ) where

import Control.DeepSeq (NFData)
import Foreign.Ptr
import Foreign.C.Types
import Isl.Types.Raw
import Isl.Map.Generated
import Isl.Foreach (RawCallback, foreachCollect, mkBasicMapCb)
import Isl.BasicMap.Generated ()  -- Consumable/Borrow BasicMap instances
import Isl.Monad.Internal (Isl, Ur)

foreign import ccall "isl_map_foreach_basic_map"
  c_map_foreach_basic_map :: Map -> FunPtr (RawCallback BasicMap) -> Ptr () -> IO CInt

-- | Iterate over BasicMaps of a Map. The Map is borrowed.
-- Each BasicMap is auto-freed after the callback returns.
foreachBasicMap
  :: NFData r
  => MapRef s
  -> (forall s'. BasicMapRef s' -> Isl (Ur r))
  -> Isl (Ur [r])
foreachBasicMap (MapRef mPtr) f =
  foreachCollect mkBasicMapCb
    (\cb -> c_map_foreach_basic_map (Map mPtr) cb nullPtr) f
