{-# LANGUAGE RankNTypes #-}

-- | Noonan's GDP primitives, minimal form.
--
-- A 'Named' wraps a value together with an opaque skolem @n@ that
-- serves as a compile-time identity tag.  The skolem is introduced
-- via rank-N CPS so callers cannot name it; distinct invocations
-- therefore bind distinct, incomparable skolems.
--
-- Alpha Core uses 'Named' as its identity layer: two nodes referring
-- to the same ISL object share a skolem, and the type system enforces
-- that equality transitively.
module Alpha.Core.Named
  ( Named
  , the
  , name
  , name2
  ) where

-- | @Named n a@: the value @a@ tagged with opaque skolem @n@.
--
-- The constructor is not exported: the only way to produce a 'Named'
-- is via 'name'.
newtype Named n a = Named a

-- | Project out the underlying value.
the :: Named n a -> a
the (Named a) = a

-- | Introduce a fresh skolem @n@ tagging @x@.
--
-- The continuation is rank-N in @n@, so the skolem escapes neither
-- the scope nor unification with any other skolem introduced by a
-- different 'name' call.
name :: a -> (forall n. Named n a -> r) -> r
name x k = k (Named x)

-- | Introduce a fresh skolem @n@ tagging @a@ and @b@ simultaneously.
--
-- The two 'Named' values share the same skolem — useful when the
-- downstream API expects two 'Named n _' fields belonging to the
-- same identity (e.g., a system's parameter list and parameter
-- constraints both tagged by the system's skolem).
name2 :: a -> b -> (forall n. Named n a -> Named n b -> r) -> r
name2 x y k = k (Named x) (Named y)
