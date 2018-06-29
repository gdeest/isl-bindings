{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Isl.Types where

import Data.Coerce
import Data.Reflection
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Unsafe.Coerce

newtype Ctx = Ctx { unCtx :: ForeignPtr Ctx }
newtype Aff = Aff (Ctx, ForeignPtr Aff)
newtype Val = Val (Ctx, ForeignPtr Val)
newtype Id = Id (Ctx, ForeignPtr Id)
newtype Set = Set (Ctx, ForeignPtr Set)
newtype BasicSet = BasicSet (Ctx, ForeignPtr BasicSet)
newtype UnionSet = UnionSet (Ctx, ForeignPtr UnionSet)
newtype Map = Map (Ctx, ForeignPtr Map)
newtype BasicMap = BasicMap (Ctx, ForeignPtr BasicMap)
newtype UnionMap = UnionMap (Ctx, ForeignPtr UnionMap)
newtype Constraint = Constraint (Ctx, ForeignPtr Constraint)
newtype Space = Space (Ctx, ForeignPtr Space)
newtype LocalSpace = LocalSpace (Ctx, ForeignPtr LocalSpace)
newtype Printer = Printer (Ctx, ForeignPtr Printer)

type RawPrinter = Ptr Printer
type RawCtx = Ptr Ctx
type RawAff = Ptr Aff
type RawVal = Ptr Val
type RawId = Ptr Id
type RawSet = Ptr Set
type RawBasicSet = Ptr BasicSet
type RawUnionSet = Ptr UnionSet
type RawMap = Ptr Map
type RawBasicMap = Ptr BasicMap
type RawUnionMap = Ptr UnionMap
type RawConstraint = Ptr Constraint
type RawSpace = Ptr Space
type RawLocalSpace = Ptr LocalSpace


foreign import ccall "isl/ctx.h isl_ctx_alloc"
    c_ctx_alloc :: IO (Ptr Ctx)

foreign import ccall "isl/ctx.h &isl_ctx_free"
    c_ctx_free :: FunPtr (Ptr Ctx -> IO ())

foreign import ccall unsafe "isl/options.h isl_options_set_on_error"
    c_options_set_on_error :: Ptr Ctx -> CInt -> IO ()

-- XXX: Need to set on context initialization as a workaround to avoid error
-- when the context is freed before all objects. Could silence legitimate
-- errors. Find a way to ensure proper order without it.
isl_on_error_continue :: CInt
isl_on_error_continue = 1

ctx_alloc :: IO Ctx
ctx_alloc = do
  ptr <- c_ctx_alloc
  c_options_set_on_error ptr isl_on_error_continue
  Ctx <$> newForeignPtr c_ctx_free ptr

-- foreign import ccall "isl/printer.h isl_printer_to_str"
--     c_isl_printer_to_str :: IO RawPrinter

-- foreign import ccall "isl/printer.h isl_printer_free"
--     c_isl_printer_free :: RawPrinter -> IO ()

-- foreign import call "isl/map.h isl_printer_print_basic_map"
--     c_isl_printer_print_basic_map ::

class IslRef a where
  islCopy :: Ptr a -> IO (Ptr a)
  islFree :: FunPtr ((Ptr a) -> IO ())

wrap' :: IslRef a => Ptr a -> IO (Maybe (ForeignPtr a))
wrap' ptr =
    if ptr == nullPtr
      then return Nothing
      else do
        fptr <- newForeignPtr islFree ptr
        return $ Just $ fptr

wrap
  :: (Given Ctx, IslRef a)
  => ((Ctx, ForeignPtr a) -> a) -> Ptr a -> IO a
wrap f ptr = do
  Just frgn <- wrap' ptr
  return $ f (given :: Ctx, frgn)

foreign import ccall unsafe "isl/basic_set.h isl_basic_set_copy"
    c_basic_set_copy :: Ptr BasicSet -> IO (Ptr BasicSet)

foreign import ccall unsafe "isl/basic_set.h &isl_basic_set_free"
    c_basic_set_free :: FunPtr (Ptr BasicSet -> IO ())

instance IslRef BasicSet where
  islCopy = c_basic_set_copy
  islFree = c_basic_set_free


foreign import ccall unsafe "isl/set.h isl_set_copy"
    c_set_copy :: Ptr Set -> IO (Ptr Set)

foreign import ccall unsafe "isl/set.h &isl_set_free"
    c_set_free :: FunPtr (Ptr Set -> IO ())

instance IslRef Set where
  islCopy = c_set_copy
  islFree = c_set_free


foreign import ccall unsafe "isl/union_set.h isl_union_set_copy"
    c_union_set_copy :: Ptr UnionSet -> IO (Ptr UnionSet)

foreign import ccall unsafe "isl/union_set.h &isl_union_set_free"
    c_union_set_free :: FunPtr (Ptr UnionSet -> IO ())

instance IslRef UnionSet where
  islCopy = c_union_set_copy
  islFree = c_union_set_free


foreign import ccall unsafe "isl/basic_map.h isl_basic_map_copy"
    c_basic_map_copy :: Ptr BasicMap -> IO (Ptr BasicMap)

foreign import ccall unsafe "isl/basic_map.h &isl_basic_map_free"
    c_basic_map_free :: FunPtr (Ptr BasicMap -> IO ())

instance IslRef BasicMap where
  islCopy = c_basic_map_copy
  islFree = c_basic_map_free


foreign import ccall unsafe "isl/map.h isl_map_copy"
    c_map_copy :: Ptr Map -> IO (Ptr Map)

foreign import ccall unsafe "isl/map.h &isl_map_free"
    c_map_free :: FunPtr (Ptr Map -> IO ())

instance IslRef Map where
  islCopy = c_map_copy
  islFree = c_map_free


foreign import ccall unsafe "isl/union_map.h isl_union_map_copy"
    c_union_map_copy :: Ptr UnionMap -> IO (Ptr UnionMap)

foreign import ccall unsafe "isl/union_map.h &isl_union_map_free"
    c_union_map_free :: FunPtr (Ptr UnionMap -> IO ())

instance IslRef UnionMap where
  islCopy = c_union_map_copy
  islFree = c_union_map_free


foreign import ccall unsafe "isl/space.h isl_space_copy"
    c_space_copy :: Ptr Space -> IO (Ptr Space)

foreign import ccall unsafe "isl/space.h &isl_space_free"
    c_space_free :: FunPtr (Ptr Space -> IO ())

instance IslRef Space where
  islCopy = c_space_copy
  islFree = c_space_free

foreign import ccall unsafe "isl/local_space.h isl_local_space_copy"
    c_local_space_copy :: Ptr LocalSpace -> IO (Ptr LocalSpace)

foreign import ccall unsafe "isl/local_space.h &isl_local_space_free"
    c_local_space_free :: FunPtr (Ptr LocalSpace -> IO ())

instance IslRef LocalSpace where
  islCopy = c_local_space_copy
  islFree = c_local_space_free



foreign import ccall unsafe "isl/id.h isl_id_copy"
    c_id_copy :: Ptr Id -> IO (Ptr Id)

foreign import ccall unsafe "isl/id.h &isl_id_free"
    c_id_free :: FunPtr (Ptr Id -> IO ())

instance IslRef Id where
  islCopy = c_id_copy
  islFree = c_id_free

foreign import ccall unsafe "isl/aff.h isl_aff_copy"
    c_aff_copy :: Ptr Aff -> IO (Ptr Aff)

foreign import ccall unsafe "isl/aff.h &isl_aff_free"
    c_aff_free :: FunPtr (Ptr Aff -> IO ())

instance IslRef Aff where
  islCopy = c_aff_copy
  islFree = c_aff_free



foreign import ccall unsafe "isl/val.h isl_val_copy"
    c_val_copy :: Ptr Val -> IO (Ptr Val)

foreign import ccall unsafe "isl/val.h &isl_val_free"
    c_val_free :: FunPtr (Ptr Val -> IO ())

instance IslRef Val where
  islCopy = c_val_copy
  islFree = c_val_free

foreign import ccall unsafe "isl/constraint.h isl_constraint_copy"
    c_constraint_copy :: Ptr Constraint -> IO (Ptr Constraint)

foreign import ccall unsafe "isl/constraint.h &isl_constraint_free"
    c_constraint_free :: FunPtr (Ptr Constraint -> IO ())

instance IslRef Constraint where
  islCopy = c_constraint_copy
  islFree = c_constraint_free
