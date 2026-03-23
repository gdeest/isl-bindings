{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linear do-notation for the 'Isl' monad via @QualifiedDo@.
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

import Prelude (($), String)
import qualified Prelude
import Isl.Monad (Isl(..), Ur(..))
import Isl.Types (Borrow(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Linear bind. With @QualifiedDo@, @x <- action@ gives @x@ multiplicity One.
(>>=) :: Isl a %1 -> (a %1 -> Isl b) %1 -> Isl b
(>>=) = unsafeCoerce bindIsl
  where
    bindIsl :: Isl a -> (a -> Isl b) -> Isl b
    bindIsl (Isl m) k = Isl $ \ctx -> do
      a <- m ctx
      unIsl (k a) ctx

-- | Linear sequence.
(>>) :: Isl () %1 -> Isl b %1 -> Isl b
m >> k = m >>= \() -> k

-- | Linear pure.
pure :: a %1 -> Isl a
pure = unsafeCoerce pureIsl
  where
    pureIsl :: a -> Isl a
    pureIsl a = Isl $ \_ -> Prelude.return a

-- | Fail (for failable pattern matches in QualifiedDo).
fail :: String -> Isl a
fail s = Isl $ \_ -> Prelude.fail s

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
borrowPure :: forall owned ref a. Borrow owned ref => owned %1 -> (ref -> a) -> Isl (Ur a, owned)
borrowPure = unsafeCoerce go
  where
    go :: owned -> (ref -> a) -> Isl (Ur a, owned)
    go owned f =
      let !(result, owned') = borrow owned f
      in Isl $ \_ -> Prelude.return (Ur result, owned')
