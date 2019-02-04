{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

module Isl.Types where

import Data.Coerce
import Data.Reflection
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce

newtype Ctx = Ctx { unCtx :: Ptr Ctx }
  deriving (Storable)
newtype Aff = Aff { unAff :: Ptr Aff }
  deriving (Storable)
newtype Val = Val { unVal :: Ptr Val }
  deriving (Storable)
newtype Id = Id { unId :: Ptr Id }
  deriving (Storable)
newtype Set = Set { unSet :: Ptr Set }
  deriving (Storable)
newtype BasicSet = BasicSet { unBasicSet :: Ptr BasicSet }
  deriving (Storable)
newtype UnionSet = UnionSet { unUnionSet :: Ptr UnionSet }
  deriving (Storable)
newtype Map = Map { unMap :: Ptr Map }
  deriving (Storable)
newtype BasicMap = BasicMap { unBasicMap :: Ptr BasicMap }
  deriving (Storable)
newtype UnionMap = UnionMap { unUnionMap :: Ptr UnionMap }
  deriving (Storable)
newtype Constraint = Constraint { unConstraint :: Ptr Constraint }
  deriving (Storable)
newtype Space = Space { unSpace :: Ptr Space }
  deriving (Storable)
newtype LocalSpace = LocalSpace { unLocalSpace :: Ptr LocalSpace }
  deriving (Storable)

foreign import ccall "isl/ctx.h isl_ctx_alloc"
    c_ctx_alloc :: IO (Ptr Ctx)

foreign import ccall "isl/ctx.h isl_ctx_free"
    c_ctx_free :: Ptr Ctx -> IO ()

#include <isl/space_type.h>

newtype DimType = DimType CInt deriving (Eq, Storable)

#enum DimType,DimType \
  , isl_dim_cst \
  , isl_dim_param \
  , isl_dim_in \
  , isl_dim_out \
  , isl_dim_set
