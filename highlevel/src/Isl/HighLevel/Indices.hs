{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Isl.HighLevel.Indices where

import GHC.TypeLits
import Unsafe.Coerce

data IxList :: Nat -> * -> * where
  Nil :: IxList 0 a
  (:-) :: a -> IxList n a -> IxList (n+1) a

infixr :-

coerceIxList :: forall n ix. SomeIxList ix -> IxList n ix
coerceIxList (SomeIxList list) = unsafeCoerce list

mkIxList :: Integer -> Integer -> SomeIxList Integer
mkIxList _ 0 = SomeIxList Nil
mkIxList start n =
  let rst = mkIxList (start+1) (n-1) in
  SomeIxList $ start :- (coerceIxList rst)

data SomeIxList ix = forall (n :: Nat).
  SomeIxList { someIxList :: (IxList n ix) }
