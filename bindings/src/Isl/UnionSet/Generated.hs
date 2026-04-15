{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.UnionSet.Generated where

import Isl.Types
import Isl.Types.Internal (Consumable(..), Borrow(..), Dupable(..))
import Isl.Monad.Internal
import Control.Monad.IO.Class (MonadIO)

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

foreign import ccall "isl_union_set_contains" c_contains :: UnionSetRef -> SpaceRef -> IO C.CInt

contains :: UnionSetRef -> SpaceRef -> Int
contains uset space =
    let !r = unsafePerformIO $ fromIntegral <$> c_contains uset space in r


foreign import ccall "isl_union_set_dim" c_dim :: UnionSetRef -> DimType -> IO C.CInt

dim :: UnionSetRef -> DimType -> Int
dim uset typ =
    let !r = unsafePerformIO $ fromIntegral <$> c_dim uset typ in r


foreign import ccall "isl_union_set_n_set" c_nSet :: UnionSetRef -> IO C.CInt

nSet :: UnionSetRef -> Int
nSet uset =
    let !r = unsafePerformIO $ fromIntegral <$> c_nSet uset in r


foreign import ccall "isl_union_set_dump" c_dump :: UnionSetRef -> IO ()

dump :: UnionSetRef -> ()
dump uset =
    let !r = unsafePerformIO $ c_dump uset in r


foreign import ccall "isl_union_set_is_params" c_isParams :: UnionSetRef -> IO C.CBool

isParams :: UnionSetRef -> Bool
isParams uset =
    let !r = unsafePerformIO $ M.toBool <$> c_isParams uset in r


foreign import ccall "isl_union_set_lex_ge_union_set" c_lexGeUnionSet :: UnionSet -> UnionSet -> IO UnionMap

lexGeUnionSet :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionMap
lexGeUnionSet = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionMap
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_lexGeUnionSet uset1 uset2


foreign import ccall "isl_union_set_lex_gt_union_set" c_lexGtUnionSet :: UnionSet -> UnionSet -> IO UnionMap

lexGtUnionSet :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionMap
lexGtUnionSet = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionMap
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_lexGtUnionSet uset1 uset2


foreign import ccall "isl_union_set_lex_le_union_set" c_lexLeUnionSet :: UnionSet -> UnionSet -> IO UnionMap

lexLeUnionSet :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionMap
lexLeUnionSet = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionMap
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_lexLeUnionSet uset1 uset2


foreign import ccall "isl_union_set_lex_lt_union_set" c_lexLtUnionSet :: UnionSet -> UnionSet -> IO UnionMap

lexLtUnionSet :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionMap
lexLtUnionSet = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionMap
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_lexLtUnionSet uset1 uset2


foreign import ccall "isl_union_set_wrapped_domain_map" c_wrappedDomainMap :: UnionSet -> IO UnionMap

wrappedDomainMap :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionMap
wrappedDomainMap = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionMap
  go uset =
    unsafeIslFromIO $ \_ -> c_wrappedDomainMap uset


foreign import ccall "isl_union_set_sample" c_sample :: UnionSet -> IO BasicSet

sample :: forall m. MonadIO m => UnionSet %1 -> IslT m BasicSet
sample = unsafeCoerce go where
  go :: UnionSet -> IslT m BasicSet
  go uset =
    unsafeIslFromIO $ \_ -> c_sample uset


foreign import ccall "isl_union_set_add_set" c_addSet :: UnionSet -> Set -> IO UnionSet

addSet :: forall m. MonadIO m => UnionSet %1 -> Set %1 -> IslT m UnionSet
addSet = unsafeCoerce go where
  go :: UnionSet -> Set -> IslT m UnionSet
  go uset set =
    unsafeIslFromIO $ \_ -> c_addSet uset set


foreign import ccall "isl_union_set_align_params" c_alignParams :: UnionSet -> Space -> IO UnionSet

alignParams :: forall m. MonadIO m => UnionSet %1 -> Space %1 -> IslT m UnionSet
alignParams = unsafeCoerce go where
  go :: UnionSet -> Space -> IslT m UnionSet
  go uset model =
    unsafeIslFromIO $ \_ -> c_alignParams uset model


foreign import ccall "isl_union_set_coefficients" c_coefficients :: UnionSet -> IO UnionSet

coefficients :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
coefficients = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go bset =
    unsafeIslFromIO $ \_ -> c_coefficients bset


foreign import ccall "isl_union_set_empty" c_empty :: Space -> IO UnionSet

empty :: forall m. MonadIO m => Space %1 -> IslT m UnionSet
empty = unsafeCoerce go where
  go :: Space -> IslT m UnionSet
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_union_set_empty_ctx" c_emptyCtx :: Ctx -> IO UnionSet

emptyCtx :: MonadIO m => IslT m UnionSet
emptyCtx =
    unsafeIslFromIO $ \ctx -> c_emptyCtx ctx


foreign import ccall "isl_union_set_empty_space" c_emptySpace :: Space -> IO UnionSet

emptySpace :: forall m. MonadIO m => Space %1 -> IslT m UnionSet
emptySpace = unsafeCoerce go where
  go :: Space -> IslT m UnionSet
  go space =
    unsafeIslFromIO $ \_ -> c_emptySpace space


foreign import ccall "isl_union_set_lift" c_lift :: UnionSet -> IO UnionSet

lift :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
lift = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_lift uset


foreign import ccall "isl_union_set_preimage_multi_aff" c_preimageMultiAff :: UnionSet -> MultiAff -> IO UnionSet

preimageMultiAff :: forall m. MonadIO m => UnionSet %1 -> MultiAff %1 -> IslT m UnionSet
preimageMultiAff = unsafeCoerce go where
  go :: UnionSet -> MultiAff -> IslT m UnionSet
  go uset ma =
    unsafeIslFromIO $ \_ -> c_preimageMultiAff uset ma


foreign import ccall "isl_union_set_preimage_pw_multi_aff" c_preimagePwMultiAff :: UnionSet -> PwMultiAff -> IO UnionSet

preimagePwMultiAff :: forall m. MonadIO m => UnionSet %1 -> PwMultiAff %1 -> IslT m UnionSet
preimagePwMultiAff = unsafeCoerce go where
  go :: UnionSet -> PwMultiAff -> IslT m UnionSet
  go uset pma =
    unsafeIslFromIO $ \_ -> c_preimagePwMultiAff uset pma


foreign import ccall "isl_union_set_product" c_product :: UnionSet -> UnionSet -> IO UnionSet

product :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
product = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionSet
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_product uset1 uset2


foreign import ccall "isl_union_set_project_out" c_projectOut :: UnionSet -> DimType -> C.CUInt -> C.CUInt -> IO UnionSet

projectOut :: forall m. MonadIO m => UnionSet %1 -> DimType -> Int -> Int -> IslT m UnionSet
projectOut = unsafeCoerce go where
  go :: UnionSet -> DimType -> Int -> Int -> IslT m UnionSet
  go uset typ first n =
    unsafeIslFromIO $ \_ -> c_projectOut uset typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_union_set_remove_divs" c_removeDivs :: UnionSet -> IO UnionSet

removeDivs :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
removeDivs = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go bset =
    unsafeIslFromIO $ \_ -> c_removeDivs bset


foreign import ccall "isl_union_set_remove_redundancies" c_removeRedundancies :: UnionSet -> IO UnionSet

removeRedundancies :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
removeRedundancies = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_removeRedundancies uset


foreign import ccall "isl_union_set_reset_user" c_resetUser :: UnionSet -> IO UnionSet

resetUser :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
resetUser = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_resetUser uset


foreign import ccall "isl_union_set_simple_hull" c_simpleHull :: UnionSet -> IO UnionSet

simpleHull :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
simpleHull = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_simpleHull uset


foreign import ccall "isl_union_set_solutions" c_solutions :: UnionSet -> IO UnionSet

solutions :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
solutions = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go bset =
    unsafeIslFromIO $ \_ -> c_solutions bset


foreign import ccall "isl_union_set_to_str" c_toStr :: UnionSetRef -> IO C.CString

toStr :: UnionSetRef -> String
toStr uset =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr uset in r


foreign import ccall "isl_union_set_isa_set" c_isaSet :: UnionSetRef -> IO C.CInt

isaSet :: UnionSetRef -> Int
isaSet uset =
    let !r = unsafePerformIO $ fromIntegral <$> c_isaSet uset in r


foreign import ccall "isl_union_set_is_disjoint" c_isDisjoint :: UnionSetRef -> UnionSetRef -> IO C.CBool

isDisjoint :: UnionSetRef -> UnionSetRef -> Bool
isDisjoint uset1 uset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isDisjoint uset1 uset2 in r


foreign import ccall "isl_union_set_is_empty" c_isEmpty :: UnionSetRef -> IO C.CBool

isEmpty :: UnionSetRef -> Bool
isEmpty uset =
    let !r = unsafePerformIO $ M.toBool <$> c_isEmpty uset in r


foreign import ccall "isl_union_set_is_equal" c_isEqual :: UnionSetRef -> UnionSetRef -> IO C.CBool

isEqual :: UnionSetRef -> UnionSetRef -> Bool
isEqual uset1 uset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isEqual uset1 uset2 in r


foreign import ccall "isl_union_set_is_strict_subset" c_isStrictSubset :: UnionSetRef -> UnionSetRef -> IO C.CBool

isStrictSubset :: UnionSetRef -> UnionSetRef -> Bool
isStrictSubset uset1 uset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isStrictSubset uset1 uset2 in r


foreign import ccall "isl_union_set_is_subset" c_isSubset :: UnionSetRef -> UnionSetRef -> IO C.CBool

isSubset :: UnionSetRef -> UnionSetRef -> Bool
isSubset uset1 uset2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isSubset uset1 uset2 in r


foreign import ccall "isl_union_set_as_set" c_asSet :: UnionSet -> IO Set

asSet :: forall m. MonadIO m => UnionSet %1 -> IslT m Set
asSet = unsafeCoerce go where
  go :: UnionSet -> IslT m Set
  go uset =
    unsafeIslFromIO $ \_ -> c_asSet uset


foreign import ccall "isl_union_set_extract_set" c_extractSet :: UnionSetRef -> Space -> IO Set

extractSet :: forall m. MonadIO m => UnionSetRef -> Space %1 -> IslT m Set
extractSet = unsafeCoerce go where
  go :: UnionSetRef -> Space -> IslT m Set
  go uset space =
    unsafeIslFromIO $ \_ -> c_extractSet uset space


foreign import ccall "isl_union_set_params" c_params :: UnionSet -> IO Set

params :: forall m. MonadIO m => UnionSet %1 -> IslT m Set
params = unsafeCoerce go where
  go :: UnionSet -> IslT m Set
  go uset =
    unsafeIslFromIO $ \_ -> c_params uset


foreign import ccall "isl_union_set_get_space" c_getSpace :: UnionSetRef -> IO Space

getSpace :: MonadIO m => UnionSetRef -> IslT m Space
getSpace uset =
    unsafeIslFromIO $ \_ -> c_getSpace uset


foreign import ccall "isl_union_set_identity" c_identity :: UnionSet -> IO UnionMap

identity :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionMap
identity = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionMap
  go uset =
    unsafeIslFromIO $ \_ -> c_identity uset


foreign import ccall "isl_union_set_unwrap" c_unwrap :: UnionSet -> IO UnionMap

unwrap :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionMap
unwrap = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionMap
  go uset =
    unsafeIslFromIO $ \_ -> c_unwrap uset


foreign import ccall "isl_union_set_affine_hull" c_affineHull :: UnionSet -> IO UnionSet

affineHull :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
affineHull = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_affineHull uset


foreign import ccall "isl_union_set_apply" c_apply :: UnionSet -> UnionMap -> IO UnionSet

apply :: forall m. MonadIO m => UnionSet %1 -> UnionMap %1 -> IslT m UnionSet
apply = unsafeCoerce go where
  go :: UnionSet -> UnionMap -> IslT m UnionSet
  go uset umap =
    unsafeIslFromIO $ \_ -> c_apply uset umap


foreign import ccall "isl_union_set_coalesce" c_coalesce :: UnionSet -> IO UnionSet

coalesce :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
coalesce = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_coalesce uset


foreign import ccall "isl_union_set_compute_divs" c_computeDivs :: UnionSet -> IO UnionSet

computeDivs :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
computeDivs = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_computeDivs uset


foreign import ccall "isl_union_set_detect_equalities" c_detectEqualities :: UnionSet -> IO UnionSet

detectEqualities :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
detectEqualities = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_detectEqualities uset


foreign import ccall "isl_union_set_drop_unused_params" c_dropUnusedParams :: UnionSet -> IO UnionSet

dropUnusedParams :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
dropUnusedParams = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams uset


foreign import ccall "isl_union_set_gist" c_gist :: UnionSet -> UnionSet -> IO UnionSet

gist :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
gist = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionSet
  go uset context =
    unsafeIslFromIO $ \_ -> c_gist uset context


foreign import ccall "isl_union_set_gist_params" c_gistParams :: UnionSet -> Set -> IO UnionSet

gistParams :: forall m. MonadIO m => UnionSet %1 -> Set %1 -> IslT m UnionSet
gistParams = unsafeCoerce go where
  go :: UnionSet -> Set -> IslT m UnionSet
  go uset set =
    unsafeIslFromIO $ \_ -> c_gistParams uset set


foreign import ccall "isl_union_set_intersect" c_intersect :: UnionSet -> UnionSet -> IO UnionSet

intersect :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
intersect = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionSet
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_intersect uset1 uset2


foreign import ccall "isl_union_set_intersect_params" c_intersectParams :: UnionSet -> Set -> IO UnionSet

intersectParams :: forall m. MonadIO m => UnionSet %1 -> Set %1 -> IslT m UnionSet
intersectParams = unsafeCoerce go where
  go :: UnionSet -> Set -> IslT m UnionSet
  go uset set =
    unsafeIslFromIO $ \_ -> c_intersectParams uset set


foreign import ccall "isl_union_set_lexmax" c_lexmax :: UnionSet -> IO UnionSet

lexmax :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
lexmax = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_lexmax uset


foreign import ccall "isl_union_set_lexmin" c_lexmin :: UnionSet -> IO UnionSet

lexmin :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
lexmin = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_lexmin uset


foreign import ccall "isl_union_set_polyhedral_hull" c_polyhedralHull :: UnionSet -> IO UnionSet

polyhedralHull :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
polyhedralHull = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_polyhedralHull uset


foreign import ccall "isl_union_set_project_out_all_params" c_projectOutAllParams :: UnionSet -> IO UnionSet

projectOutAllParams :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
projectOutAllParams = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_projectOutAllParams uset


foreign import ccall "isl_union_set_subtract" c_subtract :: UnionSet -> UnionSet -> IO UnionSet

subtract :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
subtract = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionSet
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_subtract uset1 uset2


foreign import ccall "isl_union_set_union" c_union :: UnionSet -> UnionSet -> IO UnionSet

union :: forall m. MonadIO m => UnionSet %1 -> UnionSet %1 -> IslT m UnionSet
union = unsafeCoerce go where
  go :: UnionSet -> UnionSet -> IslT m UnionSet
  go uset1 uset2 =
    unsafeIslFromIO $ \_ -> c_union uset1 uset2


foreign import ccall "isl_union_set_universe" c_universe :: UnionSet -> IO UnionSet

universe :: forall m. MonadIO m => UnionSet %1 -> IslT m UnionSet
universe = unsafeCoerce go where
  go :: UnionSet -> IslT m UnionSet
  go uset =
    unsafeIslFromIO $ \_ -> c_universe uset


foreign import ccall "isl_union_set_from_basic_set" c_fromBasicSet :: BasicSet -> IO UnionSet

fromBasicSet :: forall m. MonadIO m => BasicSet %1 -> IslT m UnionSet
fromBasicSet = unsafeCoerce go where
  go :: BasicSet -> IslT m UnionSet
  go bset =
    unsafeIslFromIO $ \_ -> c_fromBasicSet bset


foreign import ccall "isl_union_set_from_set" c_fromSet :: Set -> IO UnionSet

fromSet :: forall m. MonadIO m => Set %1 -> IslT m UnionSet
fromSet = unsafeCoerce go where
  go :: Set -> IslT m UnionSet
  go set =
    unsafeIslFromIO $ \_ -> c_fromSet set


foreign import ccall "isl_union_set_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO UnionSet

readFromStr :: MonadIO m => String -> IslT m UnionSet
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_union_set_free" c_free :: UnionSet -> IO ()

instance Consumable UnionSet where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_union_set_copy" c_copy :: UnionSet -> IO UnionSet

instance Dupable UnionSet where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow UnionSet UnionSetRef where
  borrow = unsafeCoerce $ \(UnionSet ptr) f -> let !r = f (UnionSetRef ptr) in (r, UnionSet ptr)


