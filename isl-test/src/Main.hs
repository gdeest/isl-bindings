{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding (min, max)

import Control.Monad
import qualified Isl.Types as Isl
import qualified Isl.BasicSet.AutoGen as BS
import qualified Isl.Constraint.AutoGen as Constraint
import qualified Isl.LocalSpace.AutoGen as LS
import qualified Isl.Space.AutoGen as Space

import GHC.TypeLits
import Data.Reflection
import Data.Proxy

import Foreign.C.String
import Unsafe.Coerce
import System.IO.Unsafe

newtype BasicSet (nDims :: Nat) = BSet Isl.BasicSet

data Expr ix
  = Ix ix
  | Constant Integer
  | Mul Integer (Expr ix)
  | Add (Expr ix) (Expr ix)

data Constraint ix
  = EqualityConstraint (Expr ix) -- = 0
  | InequalityConstraint (Expr ix) -- >= 0

expandExpr :: Expr Integer -> ([(Integer, Integer)], Integer)
expandExpr (Ix ix) = ([(1, ix)], 0)
expandExpr (Constant k) = ([], k)
expandExpr (Mul k e) =
  ((\(k', ix) -> (k*k', ix)) <$> linearTerms, k*co)
  where (linearTerms, co) = expandExpr e
expandExpr (Add e1 e2) = merge (expandExpr e1) (expandExpr e2)
  where merge (lt1, c1) (lt2, c2) = (mergeTerms lt1 lt2, c1+c2)
        mergeTerms [] terms = terms
        mergeTerms terms [] = terms
        mergeTerms ts1@(t1@(coeff1, ix1):rst1) ts2@(t2@(coeff2, ix2):rst2)
          | ix1 < ix2 = t1:(mergeTerms rst1 ts2)
          | ix2 < ix1 = t2:(mergeTerms ts1 rst2)
          | otherwise = (coeff1+coeff2, ix1):(mergeTerms rst1 rst2)

toBasicSet
  :: forall (n :: Nat). (Given Isl.Ctx, KnownNat n)
  => [Constraint Integer] -> BasicSet n

toBasicSet constraints = BSet $ unsafePerformIO $ do
  space <- Space.c_setAlloc given 0 (fromIntegral $ natVal $ Proxy @n)
  univ <- BS.c_universe space
  foldM addConstraint univ constraints
  where
    addConstraint bs constraint = do
      sp <- BS.c_getSpace bs
      ls <- LS.c_fromSpace sp
      (emptyConstraint, e) <-
        case constraint of
          InequalityConstraint e -> do
            co <- Constraint.c_inequalityAlloc ls
            return (co, e)
          EqualityConstraint e -> do
            co <- Constraint.c_equalityAlloc ls
            return (co, e)
      let (coeffs, constant) = expandExpr e
          setCoeff constr (coeff, ix)  = do
            Constraint.c_setCoefficientSi
              constr Isl.islDimSet (fromIntegral ix) (fromIntegral coeff)

      linearPart <- foldM setCoeff emptyConstraint coeffs
      finalConstraint <-
        Constraint.c_setConstantSi linearPart (fromIntegral constant) 

      BS.c_addConstraint bs finalConstraint

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

mkBasicSet
  :: forall (n :: Nat). (Given Isl.Ctx, KnownNat n)
  => (forall ix. IxList n ix -> [Constraint ix])
  -> BasicSet n
mkBasicSet mkConstraints = toBasicSet . mkConstraints $
  (coerceIxList $ mkIxList 0 $ natVal (Proxy @n))

range :: ix -> (Expr ix, Expr ix) -> [Constraint ix]
range ix (min, max) = InequalityConstraint <$>
  [ Add (Ix ix) (Mul (-1) min)
  , Add max (Mul (-1) (Ix ix))
  ]

c :: Integer -> Expr ix
c = Constant

i :: ix -> Expr ix
i = Ix


(&&:) :: [a] -> [a] -> [a]
(&&:) = (++)

infixl 4 -:
infixl 4 +:
infixr 5 *:

infix 3 ==:
infix 3 <=:
infix 3 >=:

infixl 2 &&:

(-:) :: Expr ix -> Expr ix -> Expr ix
e1 -: e2 = e1 +: (-1) *: e2

(+:) :: Expr ix -> Expr ix -> Expr ix
e1 +: e2 = Add e1 e2

(*:) :: Integer -> Expr ix -> Expr ix
k *: e1 = Mul k e1

(<=:) :: Expr ix -> Expr ix -> [Constraint ix]
e1 <=: e2 = [InequalityConstraint $ Add e2 (Mul (-1) e1)]

(>=:) :: Expr ix -> Expr ix -> [Constraint ix]
e1 >=: e2 = e2 <=: e1

(==:) :: Expr ix -> Expr ix -> [Constraint ix]
e1 ==: e2 = [EqualityConstraint $ Add e2 (Mul (-1) e1)]


eliminateLast
  :: forall n. (KnownNat n, 1 <= n)
  => BasicSet n -> BasicSet (n-1)
eliminateLast (BSet bs) = BSet $ unsafePerformIO $ do
  let n = (natVal $ Proxy @n) - 1
  BS.c_projectOut bs Isl.islDimSet (fromIntegral n) 1

intersect :: forall n. BasicSet n -> BasicSet n -> BasicSet n
intersect (BSet bs1) (BSet bs2) = BSet $ unsafePerformIO $ do
  bs1' <- BS.c_copy bs1
  bs2' <- BS.c_copy bs2
  BS.c_intersect bs1' bs2'

bsetToString :: forall n. BasicSet n -> String
bsetToString (BSet bs) = unsafePerformIO $
  peekCString =<< BS.c_toStr bs

-- Project out last dimension to implement modulo.
evenXs :: Given Isl.Ctx => BasicSet 2
evenXs = eliminateLast $ mkBasicSet $
  \(x :- _ :- k :- Nil) ->
    i x ==: 2 *: i k

-- { [x,y]: ) 0 <= x <= 100 && x <= y <= 100 }
mySet :: Given Isl.Ctx => BasicSet 2
mySet = mkBasicSet $
  \(x :- y :- Nil) ->
    i x >=: c 0 &&: i x <=: c 100 &&:
    i y >=: i x &&: i y <=: c 100

main :: IO ()
main = do
  ctx <- Isl.c_ctx_alloc
  give (Isl.Ctx ctx) $ do
    putStrLn $ bsetToString $ intersect mySet evenXs
