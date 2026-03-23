{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linear do-notation for the 'IslT' monad via @QualifiedDo@.
--
-- Usage:
--
-- @
-- {-\# LANGUAGE QualifiedDo \#-}
-- import qualified Isl.Linear as Isl
--
-- example = runIsl $ Isl.do
--   x <- someAction      -- x has multiplicity One
--   ...                   -- x must be consumed exactly once
-- @
--
-- Values bound by @<-@ have multiplicity One — GHC enforces that every
-- ISL object is consumed exactly once (freed, passed to an isl_take
-- function, or duplicated via 'dup').
module Isl.Linear
  ( (>>=)
  , (>>)
  , pure
  , fail
  , borrowPure
  ) where

import Prelude (($), String, Monad)
import qualified Prelude
import qualified Control.Monad.Fail as Fail
import Isl.Monad (IslT(..), Ur(..))
import Isl.Types (Borrow(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Linear bind. With @QualifiedDo@, @x <- action@ gives @x@ multiplicity One.
(>>=) :: forall m a b. Monad m => IslT m a %1 -> (a %1 -> IslT m b) %1 -> IslT m b
(>>=) = unsafeCoerce go
  where
    go :: IslT m a -> (a -> IslT m b) -> IslT m b
    go (IslT m) k = IslT $ \ctx -> do
      a <- m ctx
      unIslT (k a) ctx

-- | Linear sequence.
(>>) :: forall m b. Monad m => IslT m () %1 -> IslT m b %1 -> IslT m b
m >> k = m >>= \() -> k

-- | Linear pure.
pure :: forall m a. Monad m => a %1 -> IslT m a
pure = unsafeCoerce go
  where
    go :: a -> IslT m a
    go a = IslT $ \_ -> Prelude.return a

-- | Fail (for failable pattern matches in QualifiedDo).
fail :: Fail.MonadFail m => String -> IslT m a
fail s = IslT $ \_ -> Fail.fail s

-- | Borrow an owned value, apply a pure query, and return the result
-- wrapped in 'Ur' (unrestricted) alongside the still-owned value.
--
-- The query result is forced immediately (strict borrow) so it doesn't
-- become a dangling thunk if the owned value is later consumed.
--
-- @
-- (Ur name, x') <- borrowPure x getNameRef
-- -- name :: String (Many, safe to share)
-- -- x'   :: OwnedType (One, must be consumed)
-- @
borrowPure :: forall m owned ref a. (Monad m, Borrow owned ref) => owned %1 -> (ref -> a) -> IslT m (Ur a, owned)
borrowPure = unsafeCoerce go
  where
    go :: owned -> (ref -> a) -> IslT m (Ur a, owned)
    go owned f =
      let !(result, owned') = borrow owned f
      in IslT $ \_ -> Prelude.return (Ur result, owned')
