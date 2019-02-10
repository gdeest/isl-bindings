{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Isl.HighLevel.Context where

import Control.Exception
import Data.Proxy
import Data.Reflection
import System.IO.Unsafe

import qualified Isl.Types as Isl

type HasCtx s = Reifies s Isl.Ctx

runIsl :: (forall s. HasCtx s => Proxy s -> a) -> a
-- XXX: Freeing the context seems to have unexpected consequences.
-- It is very likely due to my liberal usage of `unsafePerformIO` and friends.
runIsl f = unsafePerformIO $ bracket Isl.c_ctx_alloc (const $ return ()) $ \ctx ->
  return $ reify (Isl.Ctx ctx) f
