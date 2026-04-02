{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
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
import Isl.Monad (IslT(..), unsafeIslFromIO)


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


-- Low-level wrappers

fromSink :: forall m. MonadIO m => UnionMap -> IslT m UnionAccessInfo
fromSink um = unsafeIslFromIO $ \_ -> c_from_sink um

setMustSource :: forall m. MonadIO m => UnionAccessInfo -> UnionMap -> IslT m UnionAccessInfo
setMustSource ai um = unsafeIslFromIO $ \_ -> c_set_must_source ai um

setScheduleMap :: forall m. MonadIO m => UnionAccessInfo -> UnionMap -> IslT m UnionAccessInfo
setScheduleMap ai um = unsafeIslFromIO $ \_ -> c_set_schedule_map ai um

computeFlow :: forall m. MonadIO m => UnionAccessInfo -> IslT m UnionFlow
computeFlow ai = unsafeIslFromIO $ \_ -> c_compute_flow ai

getMustDependence :: forall m. MonadIO m => UnionFlow -> IslT m UnionMap
getMustDependence (UnionFlow ptr) = unsafeIslFromIO $ \_ -> c_get_must_dep (UnionFlow ptr)

getMayDependence :: forall m. MonadIO m => UnionFlow -> IslT m UnionMap
getMayDependence (UnionFlow ptr) = unsafeIslFromIO $ \_ -> c_get_may_dep (UnionFlow ptr)

freeAccessInfo :: forall m. MonadIO m => UnionAccessInfo -> IslT m ()
freeAccessInfo ai = IslT $ \_ -> liftIO (c_free_access_info ai)

freeFlow :: forall m. MonadIO m => UnionFlow -> IslT m ()
freeFlow fl = IslT $ \_ -> liftIO (c_free_flow fl)


-- | Compute flow dependencies from read/write access relations and a schedule.
--
-- All ISL objects are created and consumed within this call.
-- The returned 'UnionMap' contains the must-dependence relation:
-- @{ source_instance -> sink_instance }@
--
-- Example for Jacobi 2D:
-- @
--   reads  = { S[t,i,j] -> A[t-1,i-1,j]; S[t,i,j] -> A[t-1,i+1,j]; ... }
--   writes = { S[t,i,j] -> A[t,i,j] }
--   sched  = { S[t,i,j] -> [t,i,j] }  -- identity
--   deps   = computeFlowDeps reads writes sched
--   -- Result: { S[t,i,j] -> S[t+1,i',j'] } for each neighbor
-- @
computeFlowDeps :: forall m. MonadIO m
  => UnionMap   -- ^ read access relations (sink) — consumed
  -> UnionMap   -- ^ write access relations (must_source) — consumed
  -> UnionMap   -- ^ schedule map — consumed
  -> IslT m UnionMap  -- ^ must-dependence relation
computeFlowDeps reads writes sched = do
  ai <- fromSink reads
  ai' <- setMustSource ai writes
  ai'' <- setScheduleMap ai' sched
  flow <- computeFlow ai''
  deps <- getMustDependence flow
  freeFlow flow
  return deps
