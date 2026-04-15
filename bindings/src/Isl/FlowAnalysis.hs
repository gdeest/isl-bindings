{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ISL dataflow analysis: automatic dependency derivation from access relations.
--
-- Given READ and WRITE access relations, ISL computes exact flow dependencies.
-- This replaces manual dependency enumeration for stencils and other regular programs.
--
-- Key pipeline:
--   1. @fromSink@ — create analysis from read access relation
--   2. @setMustSource@ — set write access relation
--   3. @setScheduleMap@ — set execution schedule (identity for unscheduled)
--   4. @computeFlow@ — derive dependencies
--   5. @getMustDependence@ — extract exact flow deps as union map
module Isl.FlowAnalysis
  ( -- * High-level
    computeFlowDeps
    -- * Low-level
  , fromSink
  , setMustSource
  , setScheduleMap
  , computeFlow
  , getMustDependence
  , getMayDependence
  , freeAccessInfo
  , freeFlow
  ) where

import Foreign.Ptr (Ptr)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (free)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Isl.Types
import Isl.Monad.Internal (IslT(..), unsafeIslFromIO)
import qualified Isl.Linear as Isl


-- Opaque types
newtype UnionAccessInfo = UnionAccessInfo (Ptr UnionAccessInfo)
newtype UnionFlow = UnionFlow (Ptr UnionFlow)

-- Raw C imports
foreign import ccall "isl_union_access_info_from_sink"
  c_from_sink :: UnionMap -> IO UnionAccessInfo

foreign import ccall "isl_union_access_info_set_must_source"
  c_set_must_source :: UnionAccessInfo -> UnionMap -> IO UnionAccessInfo

foreign import ccall "isl_union_access_info_set_schedule_map"
  c_set_schedule_map :: UnionAccessInfo -> UnionMap -> IO UnionAccessInfo

foreign import ccall "isl_union_access_info_compute_flow"
  c_compute_flow :: UnionAccessInfo -> IO UnionFlow

foreign import ccall "isl_union_flow_get_must_dependence"
  c_get_must_dep :: UnionFlow -> IO UnionMap

foreign import ccall "isl_union_flow_get_may_dependence"
  c_get_may_dep :: UnionFlow -> IO UnionMap

foreign import ccall "isl_union_access_info_free"
  c_free_access_info :: UnionAccessInfo -> IO ()

foreign import ccall "isl_union_flow_free"
  c_free_flow :: UnionFlow -> IO ()


-- Low-level wrappers (all __isl_take → %1)

fromSink :: forall m. MonadIO m => UnionMap %1 -> IslT m UnionAccessInfo
fromSink = unsafeCoerce go where
  go :: UnionMap -> IslT m UnionAccessInfo
  go um = unsafeIslFromIO $ \_ -> c_from_sink um

setMustSource :: forall m. MonadIO m => UnionAccessInfo %1 -> UnionMap %1 -> IslT m UnionAccessInfo
setMustSource = unsafeCoerce go where
  go :: UnionAccessInfo -> UnionMap -> IslT m UnionAccessInfo
  go ai um = unsafeIslFromIO $ \_ -> c_set_must_source ai um

setScheduleMap :: forall m. MonadIO m => UnionAccessInfo %1 -> UnionMap %1 -> IslT m UnionAccessInfo
setScheduleMap = unsafeCoerce go where
  go :: UnionAccessInfo -> UnionMap -> IslT m UnionAccessInfo
  go ai um = unsafeIslFromIO $ \_ -> c_set_schedule_map ai um

computeFlow :: forall m. MonadIO m => UnionAccessInfo %1 -> IslT m UnionFlow
computeFlow = unsafeCoerce go where
  go :: UnionAccessInfo -> IslT m UnionFlow
  go ai = unsafeIslFromIO $ \_ -> c_compute_flow ai

-- | Get the must-dependence relation. __isl_keep: returns the flow alongside the result.
getMustDependence :: forall m. MonadIO m => UnionFlow %1 -> IslT m (UnionMap, UnionFlow)
getMustDependence = unsafeCoerce go where
  go :: UnionFlow -> IslT m (UnionMap, UnionFlow)
  go flow@(UnionFlow ptr) = unsafeIslFromIO $ \_ -> do
    deps <- c_get_must_dep (UnionFlow ptr)
    return (deps, flow)

-- | Get the may-dependence relation. __isl_keep: returns the flow alongside the result.
getMayDependence :: forall m. MonadIO m => UnionFlow %1 -> IslT m (UnionMap, UnionFlow)
getMayDependence = unsafeCoerce go where
  go :: UnionFlow -> IslT m (UnionMap, UnionFlow)
  go flow@(UnionFlow ptr) = unsafeIslFromIO $ \_ -> do
    deps <- c_get_may_dep (UnionFlow ptr)
    return (deps, flow)

freeAccessInfo :: forall m. MonadIO m => UnionAccessInfo %1 -> IslT m ()
freeAccessInfo = unsafeCoerce go where
  go :: UnionAccessInfo -> IslT m ()
  go ai = IslT $ \_ -> liftIO (c_free_access_info ai)

freeFlow :: forall m. MonadIO m => UnionFlow %1 -> IslT m ()
freeFlow = unsafeCoerce go where
  go :: UnionFlow -> IslT m ()
  go fl = IslT $ \_ -> liftIO (c_free_flow fl)


-- | Compute flow dependencies from read/write access relations and a schedule.
--
-- All ISL objects are created and consumed within this call.
-- The returned 'UnionMap' contains the must-dependence relation:
-- @{ source_instance -> sink_instance }@
computeFlowDeps :: forall m. MonadIO m
  => UnionMap %1  -- ^ read access relations (sink) — consumed
  -> UnionMap %1  -- ^ write access relations (must_source) — consumed
  -> UnionMap %1  -- ^ schedule map — consumed
  -> IslT m UnionMap  -- ^ must-dependence relation
computeFlowDeps reads writes sched = Isl.do
  ai   <- fromSink reads
  ai'  <- setMustSource ai writes
  ai'' <- setScheduleMap ai' sched
  flow <- computeFlow ai''
  (deps, flow') <- getMustDependence flow
  freeFlow flow'
  Isl.pure deps
