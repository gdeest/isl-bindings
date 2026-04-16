{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Type-safe kernel interface with curried I/O and typed parameters.
module Alpha.Kernel
  ( -- * Typed kernel
    TypedKernel(..)
    -- * Parameters
  , Params(..)
  , paramsToList
    -- * Curried run
  , runKernel
    -- * Typed compile
  , withCompiledKernel
    -- * Type families
  , CurriedRun
  , Outputs
  , AllScalar
  ) where

import Control.Exception (bracket)
import Data.Kind (Type, Constraint)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Unboxed as V
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import System.Posix.DynamicLinker (dlclose)

import Alpha.Core
  ( VarDecl, DeclType, DeclName
  , DeclList(Nil), Decl(MkDecl)
  , Decls(Decls, dInputs, dOutputs, dLocals)
  , System, pattern System )
import qualified Alpha.Core as Core
import Alpha.Codegen.Compile
  ( CompiledKernel(..), compileKernel, executeKernelHet )
import Alpha.Codegen.FunctionMapping (CFunctionMapping)
import Alpha.Schedule (Schedule)
import Alpha.Allocation (Allocation)
import Alpha.Scalar
  ( ScalarDesc, AlphaScalar(..), scalarDesc, SomeBuffer(..), toSomeBuffer, fromSomeBuffer )
import Isl.Typed.Params (KnownSymbols)


-- ═══════════════════════════════════════════════════════════════════════
-- §1. TypedKernel
-- ═══════════════════════════════════════════════════════════════════════

data TypedKernel (ps :: [Symbol]) (inputs :: [VarDecl ps]) (outputs :: [VarDecl ps])
  = TypedKernel { tkUntyped :: !CompiledKernel }


-- ═══════════════════════════════════════════════════════════════════════
-- §2. Params
-- ═══════════════════════════════════════════════════════════════════════

data Params (ps :: [Symbol]) where
  PNil :: Params '[]
  (:>) :: !Int -> !(Params rest) -> Params (p ': rest)
infixr 5 :>

paramsToList :: Params ps -> [Int]
paramsToList PNil = []
paramsToList (v :> rest) = v : paramsToList rest


-- ═══════════════════════════════════════════════════════════════════════
-- §3. Type families
-- ═══════════════════════════════════════════════════════════════════════

type family Outputs (outs :: [VarDecl ps]) :: Type where
  Outputs '[]  = ()
  Outputs '[d] = V.Vector (DeclType d)
  Outputs (d ': rest) = (V.Vector (DeclType d), Outputs rest)

type family CurriedRun (ins :: [VarDecl ps]) (outs :: [VarDecl ps]) :: Type where
  CurriedRun '[] outs = IO (Outputs outs)
  CurriedRun (d ': rest) outs = V.Vector (DeclType d) -> CurriedRun rest outs

type family AllScalar (ds :: [VarDecl ps]) :: Constraint where
  AllScalar '[] = ()
  AllScalar (d ': rest) = (AlphaScalar (DeclType d), V.Unbox (DeclType d), AllScalar rest)


-- ═══════════════════════════════════════════════════════════════════════
-- §4. Heterogeneous input accumulator
-- ═══════════════════════════════════════════════════════════════════════

data HInputs (ds :: [VarDecl ps]) where
  HNil  :: HInputs '[]
  HCons :: (AlphaScalar (DeclType d), V.Unbox (DeclType d))
        => V.Vector (DeclType d) -> HInputs rest -> HInputs (d ': rest)

marshalHInputs :: HInputs ds -> IO [SomeBuffer]
marshalHInputs HNil = pure []
marshalHInputs (HCons vec rest) = do
  buf <- toSomeBuffer vec
  bufs <- marshalHInputs rest
  pure (buf : bufs)


-- ═══════════════════════════════════════════════════════════════════════
-- §5. CurryInputs
-- ═══════════════════════════════════════════════════════════════════════

class CurryInputs (ins :: [VarDecl ps]) (outs :: [VarDecl ps]) where
  curryInputs :: (HInputs ins -> IO (Outputs outs)) -> CurriedRun ins outs

instance CurryInputs ('[] :: [VarDecl ps]) outs where
  curryInputs k = k HNil

instance ( AlphaScalar (DeclType d), V.Unbox (DeclType d)
         , CurryInputs rest outs
         ) => CurryInputs ((d ': rest) :: [VarDecl ps]) outs where
  curryInputs k = \vec -> curryInputs @_ @rest @outs (\rest -> k (HCons vec rest))


-- ═══════════════════════════════════════════════════════════════════════
-- §6. ReadOutputs
-- ═══════════════════════════════════════════════════════════════════════

class ReadOutputs (outs :: [VarDecl ps]) where
  readOutputs :: [SomeBuffer] -> IO (Outputs outs)

instance ReadOutputs ('[] :: [VarDecl ps]) where
  readOutputs _ = pure ()

instance (AlphaScalar (DeclType d), V.Unbox (DeclType d))
  => ReadOutputs ('[d] :: [VarDecl ps]) where
  readOutputs (b : _) = fromSomeBuffer b
  readOutputs []      = error "Alpha.Kernel.readOutputs: missing output buffer"

instance ( AlphaScalar (DeclType d), V.Unbox (DeclType d)
         , ReadOutputs (d2 ': rest)
         ) => ReadOutputs ((d ': d2 ': rest) :: [VarDecl ps]) where
  readOutputs (b : bs) = do
    v <- fromSomeBuffer b
    vs <- readOutputs @_ @(d2 ': rest) bs
    pure (v, vs)
  readOutputs [] = error "Alpha.Kernel.readOutputs: missing output buffer"


-- ═══════════════════════════════════════════════════════════════════════
-- §7. CollectDescs
-- ═══════════════════════════════════════════════════════════════════════

class CollectDescs (ds :: [VarDecl ps]) where
  collectDescsFromList :: DeclList ps ds -> Map String ScalarDesc

instance CollectDescs ('[] :: [VarDecl ps]) where
  collectDescsFromList Nil = Map.empty

instance ( AlphaScalar (DeclType d), KnownSymbol (DeclName d)
         , CollectDescs rest
         ) => CollectDescs ((d ': rest) :: [VarDecl ps]) where
  collectDescsFromList ((MkDecl :: Decl ps d) Core.:> rest) =
    Map.insert
      (symbolVal (Proxy @(DeclName d)))
      (scalarDesc @(DeclType d))
      (collectDescsFromList rest)

collectAllDescs
  :: (CollectDescs inputs, CollectDescs outputs, CollectDescs locals)
  => System ps inputs outputs locals -> Map String ScalarDesc
collectAllDescs (System decls _) =
  Map.unions
    [ collectDescsFromList (dInputs decls)
    , collectDescsFromList (dOutputs decls)
    , collectDescsFromList (dLocals decls)
    ]


-- ═══════════════════════════════════════════════════════════════════════
-- §8. Typed runKernel
-- ═══════════════════════════════════════════════════════════════════════

runKernel
  :: forall ps inputs outputs.
     ( CurryInputs inputs outputs
     , ReadOutputs outputs
     )
  => TypedKernel ps inputs outputs
  -> Params ps
  -> CurriedRun inputs outputs
runKernel (TypedKernel ck) params =
  curryInputs @_ @inputs @outputs $ \hinputs -> do
    let paramVals = paramsToList params
    inputBufs <- marshalHInputs hinputs
    outputBufs <- executeKernelHet ck paramVals inputBufs
    readOutputs @_ @outputs outputBufs


-- ═══════════════════════════════════════════════════════════════════════
-- §9. Typed withCompiledKernel
-- ═══════════════════════════════════════════════════════════════════════

withCompiledKernel
  :: forall ps inputs outputs locals r.
     ( KnownSymbols ps
     , AllScalar inputs, AllScalar outputs, AllScalar locals
     , CollectDescs inputs, CollectDescs outputs, CollectDescs locals
     )
  => System ps inputs outputs locals
  -> Schedule -> Allocation -> CFunctionMapping
  -> (TypedKernel ps inputs outputs -> IO r) -> IO r
withCompiledKernel sys sched alloc fmap' k = do
  let descs = collectAllDescs sys
  bracket
    (compileKernel sys descs sched alloc fmap')
    (dlclose . ckHandle)
    (\ck -> k (TypedKernel ck))
