{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

-- | Manual FFI bindings for callback-based iteration and value extraction.
-- These functions cannot be auto-generated because the codegen does not
-- handle callback (function pointer) parameters.
module Isl.Foreach
  ( -- * Val extraction
    valGetNumSi
  , valGetDenSi
    -- * Space queries
  , spaceDim
  , spaceFree
  , basicSetGetSpace
  , basicMapGetSpace
  , setGetSpace
  , mapGetSpace
    -- * String conversion
  , basicSetToStr
  , setToStr
  , basicMapToStr
  , mapToStr
  , unionSetToStr
  , unionMapToStr
    -- * Constraint inspection and cleanup
  , constraintFree
  , constraintIsEquality
  , constraintGetCoefficientSi
  , constraintGetConstantSi
    -- * BasicSet iteration
  , basicSetForeachConstraint
    -- * BasicSet free
  , basicSetFree
    -- * Set iteration
  , setForeachBasicSet
    -- * Set free
  , setFree
    -- * UnionSet iteration
  , unionSetForeachSet
    -- * BasicMap iteration
  , basicMapForeachConstraint
    -- * BasicMap free
  , basicMapFree
    -- * Map iteration
  , mapForeachBasicMap
    -- * Map free
  , mapFree
    -- * UnionMap iteration
  , unionMapForeachMap
  ) where

import Data.IORef
import Foreign.C.Types
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr
import Control.Exception (bracket)

import Isl.Types

------------------------------------------------------------------------
-- Val extraction
------------------------------------------------------------------------

foreign import ccall "isl_val_get_num_si"
  c_val_get_num_si :: Val -> IO CLong

foreign import ccall "isl_val_get_den_si"
  c_val_get_den_si :: Val -> IO CLong

foreign import ccall "isl_val_free"
  c_val_free :: Val -> IO ()

-- | Extract the numerator of an ISL Val as an Integer.
-- The Val is __isl_keep — not consumed.
valGetNumSi :: Val -> IO Integer
valGetNumSi v = fromIntegral <$> c_val_get_num_si v

-- | Extract the denominator of an ISL Val as an Integer.
-- The Val is __isl_keep — not consumed.
valGetDenSi :: Val -> IO Integer
valGetDenSi v = fromIntegral <$> c_val_get_den_si v

------------------------------------------------------------------------
-- Space queries
------------------------------------------------------------------------

foreign import ccall "isl_space_dim"
  c_space_dim :: Space -> CInt -> IO CUInt

-- | Get the number of dimensions of the given type in a Space.
spaceDim :: Space -> DimType -> IO Int
spaceDim sp (DimType dt) = fromIntegral <$> c_space_dim sp dt

foreign import ccall "isl_space_free"
  c_space_free :: Space -> IO ()

spaceFree :: Space -> IO ()
spaceFree = c_space_free

------------------------------------------------------------------------
-- Space extraction from objects
------------------------------------------------------------------------

foreign import ccall "isl_basic_set_get_space"
  c_basic_set_get_space :: BasicSet -> IO Space

-- | Get the space of a BasicSet. The BasicSet is __isl_keep.
-- The returned Space is __isl_give — caller must free it.
basicSetGetSpace :: BasicSetRef -> IO Space
basicSetGetSpace (BasicSetRef bsPtr) = c_basic_set_get_space (BasicSet bsPtr)

foreign import ccall "isl_basic_map_get_space"
  c_basic_map_get_space :: BasicMap -> IO Space

basicMapGetSpace :: BasicMapRef -> IO Space
basicMapGetSpace (BasicMapRef bmPtr) = c_basic_map_get_space (BasicMap bmPtr)

foreign import ccall "isl_set_get_space"
  c_set_get_space :: Set -> IO Space

-- | Get the space of a Set. The Set is __isl_keep.
setGetSpace :: SetRef -> IO Space
setGetSpace (SetRef sPtr) = c_set_get_space (Set sPtr)

foreign import ccall "isl_map_get_space"
  c_map_get_space :: Map -> IO Space

-- | Get the space of a Map. The Map is __isl_keep.
mapGetSpace :: MapRef -> IO Space
mapGetSpace (MapRef mPtr) = c_map_get_space (Map mPtr)

------------------------------------------------------------------------
-- String conversion
------------------------------------------------------------------------

foreign import ccall "isl_basic_set_to_str"
  c_basic_set_to_str :: BasicSet -> IO CString

-- | Convert a BasicSet to its ISL string representation.
-- The BasicSet is __isl_keep.
basicSetToStr :: BasicSetRef -> IO String
basicSetToStr (BasicSetRef bsPtr) =
  c_basic_set_to_str (BasicSet bsPtr) >>= peekCString

foreign import ccall "isl_set_to_str"
  c_set_to_str :: Set -> IO CString

setToStr :: SetRef -> IO String
setToStr (SetRef sPtr) =
  c_set_to_str (Set sPtr) >>= peekCString

foreign import ccall "isl_basic_map_to_str"
  c_basic_map_to_str :: BasicMap -> IO CString

basicMapToStr :: BasicMapRef -> IO String
basicMapToStr (BasicMapRef bmPtr) =
  c_basic_map_to_str (BasicMap bmPtr) >>= peekCString

foreign import ccall "isl_map_to_str"
  c_map_to_str :: Map -> IO CString

mapToStr :: MapRef -> IO String
mapToStr (MapRef mPtr) =
  c_map_to_str (Map mPtr) >>= peekCString

foreign import ccall "isl_union_set_to_str"
  c_union_set_to_str :: UnionSet -> IO CString

unionSetToStr :: UnionSetRef -> IO String
unionSetToStr (UnionSetRef usPtr) =
  c_union_set_to_str (UnionSet usPtr) >>= peekCString

foreign import ccall "isl_union_map_to_str"
  c_union_map_to_str :: UnionMap -> IO CString

unionMapToStr :: UnionMapRef -> IO String
unionMapToStr (UnionMapRef umPtr) =
  c_union_map_to_str (UnionMap umPtr) >>= peekCString

------------------------------------------------------------------------
-- Constraint free
------------------------------------------------------------------------

foreign import ccall "isl_constraint_free"
  c_constraint_free :: Constraint -> IO ()

-- | Free a Constraint. Used after extracting coefficient data in callbacks.
constraintFree :: Constraint -> IO ()
constraintFree = c_constraint_free

------------------------------------------------------------------------
-- Constraint inspection
------------------------------------------------------------------------

foreign import ccall "isl_constraint_is_equality"
  c_constraint_is_equality :: Constraint -> IO CInt

foreign import ccall "isl_constraint_get_coefficient_val"
  c_constraint_get_coefficient_val :: Constraint -> CInt -> CInt -> IO Val

foreign import ccall "isl_constraint_get_constant_val"
  c_constraint_get_constant_val :: Constraint -> IO Val

-- | Check whether a constraint is an equality (vs inequality).
constraintIsEquality :: Constraint -> IO Bool
constraintIsEquality c = (/= 0) <$> c_constraint_is_equality c

-- | Get the coefficient of a dimension as an Integer. Extracts the Val
-- and frees it, returning just the numerator (asserts denominator == 1).
constraintGetCoefficientSi :: Constraint -> DimType -> Int -> IO Integer
constraintGetCoefficientSi c (DimType dt) pos = do
  val <- c_constraint_get_coefficient_val c dt (fromIntegral pos)
  n <- valGetNumSi val
  c_val_free val
  return n

-- | Get the constant term of a constraint as an Integer.
constraintGetConstantSi :: Constraint -> IO Integer
constraintGetConstantSi c = do
  val <- c_constraint_get_constant_val c
  n <- valGetNumSi val
  c_val_free val
  return n

------------------------------------------------------------------------
-- Free functions for types received in callbacks
------------------------------------------------------------------------

foreign import ccall "isl_basic_set_free"
  c_basic_set_free :: BasicSet -> IO ()

basicSetFree :: BasicSet -> IO ()
basicSetFree = c_basic_set_free

foreign import ccall "isl_set_free"
  c_set_free :: Set -> IO ()

setFree :: Set -> IO ()
setFree = c_set_free

foreign import ccall "isl_basic_map_free"
  c_basic_map_free :: BasicMap -> IO ()

basicMapFree :: BasicMap -> IO ()
basicMapFree = c_basic_map_free

foreign import ccall "isl_map_free"
  c_map_free :: Map -> IO ()

mapFree :: Map -> IO ()
mapFree = c_map_free

------------------------------------------------------------------------
-- BasicSet foreach constraint
------------------------------------------------------------------------

type RawCallback a = a -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  mkConstraintCb :: RawCallback Constraint -> IO (FunPtr (RawCallback Constraint))

foreign import ccall "isl_basic_set_foreach_constraint"
  c_basic_set_foreach_constraint :: BasicSet -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicSet. The BasicSet is borrowed
-- (__isl_keep). Each Constraint passed to the callback is __isl_take —
-- the callback owns it and must free it.
basicSetForeachConstraint :: BasicSetRef -> (Constraint -> IO a) -> IO [a]
basicSetForeachConstraint (BasicSetRef bsPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_set_foreach_constraint (BasicSet bsPtr) cb nullPtr) f

------------------------------------------------------------------------
-- Set foreach basic_set
------------------------------------------------------------------------

foreign import ccall "wrapper"
  mkBasicSetCb :: RawCallback BasicSet -> IO (FunPtr (RawCallback BasicSet))

foreign import ccall "isl_set_foreach_basic_set"
  c_set_foreach_basic_set :: Set -> FunPtr (RawCallback BasicSet) -> Ptr () -> IO CInt

-- | Iterate over BasicSets of a Set. The Set is borrowed (__isl_keep).
-- Each BasicSet passed to the callback is __isl_take.
setForeachBasicSet :: SetRef -> (BasicSet -> IO a) -> IO [a]
setForeachBasicSet (SetRef sPtr) f =
  foreachCollect mkBasicSetCb
    (\cb -> c_set_foreach_basic_set (Set sPtr) cb nullPtr) f

------------------------------------------------------------------------
-- UnionSet foreach set
------------------------------------------------------------------------

foreign import ccall "wrapper"
  mkSetCb :: RawCallback Set -> IO (FunPtr (RawCallback Set))

foreign import ccall "isl_union_set_foreach_set"
  c_union_set_foreach_set :: UnionSet -> FunPtr (RawCallback Set) -> Ptr () -> IO CInt

-- | Iterate over Sets of a UnionSet. The UnionSet is borrowed (__isl_keep).
-- Each Set passed to the callback is __isl_take.
unionSetForeachSet :: UnionSetRef -> (Set -> IO a) -> IO [a]
unionSetForeachSet (UnionSetRef usPtr) f =
  foreachCollect mkSetCb
    (\cb -> c_union_set_foreach_set (UnionSet usPtr) cb nullPtr) f

------------------------------------------------------------------------
-- BasicMap foreach constraint
------------------------------------------------------------------------

foreign import ccall "isl_basic_map_foreach_constraint"
  c_basic_map_foreach_constraint :: BasicMap -> FunPtr (RawCallback Constraint) -> Ptr () -> IO CInt

-- | Iterate over constraints of a BasicMap. The BasicMap is borrowed.
-- Each Constraint is __isl_take.
basicMapForeachConstraint :: BasicMapRef -> (Constraint -> IO a) -> IO [a]
basicMapForeachConstraint (BasicMapRef bmPtr) f =
  foreachCollect mkConstraintCb
    (\cb -> c_basic_map_foreach_constraint (BasicMap bmPtr) cb nullPtr) f

------------------------------------------------------------------------
-- Map foreach basic_map
------------------------------------------------------------------------

foreign import ccall "wrapper"
  mkBasicMapCb :: RawCallback BasicMap -> IO (FunPtr (RawCallback BasicMap))

foreign import ccall "isl_map_foreach_basic_map"
  c_map_foreach_basic_map :: Map -> FunPtr (RawCallback BasicMap) -> Ptr () -> IO CInt

-- | Iterate over BasicMaps of a Map. The Map is borrowed.
-- Each BasicMap is __isl_take.
mapForeachBasicMap :: MapRef -> (BasicMap -> IO a) -> IO [a]
mapForeachBasicMap (MapRef mPtr) f =
  foreachCollect mkBasicMapCb
    (\cb -> c_map_foreach_basic_map (Map mPtr) cb nullPtr) f

------------------------------------------------------------------------
-- UnionMap foreach map
------------------------------------------------------------------------

foreign import ccall "wrapper"
  mkMapCb :: RawCallback Map -> IO (FunPtr (RawCallback Map))

foreign import ccall "isl_union_map_foreach_map"
  c_union_map_foreach_map :: UnionMap -> FunPtr (RawCallback Map) -> Ptr () -> IO CInt

-- | Iterate over Maps of a UnionMap. The UnionMap is borrowed.
-- Each Map is __isl_take.
unionMapForeachMap :: UnionMapRef -> (Map -> IO a) -> IO [a]
unionMapForeachMap (UnionMapRef umPtr) f =
  foreachCollect mkMapCb
    (\cb -> c_union_map_foreach_map (UnionMap umPtr) cb nullPtr) f

------------------------------------------------------------------------
-- Internal: generic foreach → list collection
------------------------------------------------------------------------

-- | Generic foreach collection pattern. Creates a FunPtr callback that
-- applies @process@ to each element, accumulating results in an IORef.
-- The FunPtr is freed even if an exception occurs.
foreachCollect
  :: (forall b. (a -> Ptr () -> IO CInt) -> IO (FunPtr (a -> Ptr () -> IO CInt)))
     -- ^ wrapper function (from @foreign import ccall "wrapper"@)
  -> (FunPtr (a -> Ptr () -> IO CInt) -> IO CInt)
     -- ^ the C foreach call, partially applied with the ISL object
  -> (a -> IO r)
     -- ^ process each element (element is __isl_take — must be freed by caller)
  -> IO [r]
foreachCollect mkWrapper doForeach process = do
  ref <- newIORef []
  bracket
    (mkWrapper $ \element _user -> do
      result <- process element
      modifyIORef' ref (result :)
      return 0)  -- isl_stat_ok
    freeHaskellFunPtr
    (\cb -> do
      _ <- doForeach cb
      reverse <$> readIORef ref)
