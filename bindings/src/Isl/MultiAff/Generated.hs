{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.MultiAff.Generated where

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

foreign import ccall "isl_multi_aff_dim" c_dim :: MultiAffRef -> DimType -> IO C.CInt

dim :: MultiAffRef -> DimType -> Int
dim multi typ =
    let !r = unsafePerformIO $ fromIntegral <$> c_dim multi typ in r


foreign import ccall "isl_multi_aff_find_dim_by_id" c_findDimById :: MultiAffRef -> DimType -> IdRef -> IO C.CInt

findDimById :: MultiAffRef -> DimType -> IdRef -> Int
findDimById multi typ id =
    let !r = unsafePerformIO $ fromIntegral <$> c_findDimById multi typ id in r


foreign import ccall "isl_multi_aff_find_dim_by_name" c_findDimByName :: MultiAffRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: MultiAffRef -> DimType -> String -> Int
findDimByName multi typ name =
    let !r = unsafePerformIO $ do
          name_c <- C.newCString name
          fromIntegral <$> c_findDimByName multi typ name_c
    in r


foreign import ccall "isl_multi_aff_involves_dims" c_involvesDims :: MultiAffRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: MultiAffRef -> DimType -> Int -> Int -> Int
involvesDims multi typ first n =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesDims multi typ (fromIntegral first) (fromIntegral n) in r


foreign import ccall "isl_multi_aff_plain_cmp" c_plainCmp :: MultiAffRef -> MultiAffRef -> IO C.CInt

plainCmp :: MultiAffRef -> MultiAffRef -> Int
plainCmp multi1 multi2 =
    let !r = unsafePerformIO $ fromIntegral <$> c_plainCmp multi1 multi2 in r


foreign import ccall "isl_multi_aff_dump" c_dump :: MultiAffRef -> IO ()

dump :: MultiAffRef -> ()
dump maff =
    let !r = unsafePerformIO $ c_dump maff in r


foreign import ccall "isl_multi_aff_get_tuple_name" c_getTupleName :: MultiAffRef -> DimType -> IO C.CString

getTupleName :: MultiAffRef -> DimType -> String
getTupleName multi typ =
    let !r = unsafePerformIO $ C.peekCString =<< c_getTupleName multi typ in r


foreign import ccall "isl_multi_aff_has_tuple_id" c_hasTupleId :: MultiAffRef -> DimType -> IO C.CBool

hasTupleId :: MultiAffRef -> DimType -> Bool
hasTupleId multi typ =
    let !r = unsafePerformIO $ M.toBool <$> c_hasTupleId multi typ in r


foreign import ccall "isl_multi_aff_range_is_wrapping" c_rangeIsWrapping :: MultiAffRef -> IO C.CBool

rangeIsWrapping :: MultiAffRef -> Bool
rangeIsWrapping multi =
    let !r = unsafePerformIO $ M.toBool <$> c_rangeIsWrapping multi in r


foreign import ccall "isl_multi_aff_lex_ge_set" c_lexGeSet :: MultiAff -> MultiAff -> IO Set

lexGeSet :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m Set
lexGeSet = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m Set
  go ma1 ma2 =
    unsafeIslFromIO $ \_ -> c_lexGeSet ma1 ma2


foreign import ccall "isl_multi_aff_lex_gt_set" c_lexGtSet :: MultiAff -> MultiAff -> IO Set

lexGtSet :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m Set
lexGtSet = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m Set
  go ma1 ma2 =
    unsafeIslFromIO $ \_ -> c_lexGtSet ma1 ma2


foreign import ccall "isl_multi_aff_lex_le_set" c_lexLeSet :: MultiAff -> MultiAff -> IO Set

lexLeSet :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m Set
lexLeSet = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m Set
  go ma1 ma2 =
    unsafeIslFromIO $ \_ -> c_lexLeSet ma1 ma2


foreign import ccall "isl_multi_aff_lex_lt_set" c_lexLtSet :: MultiAff -> MultiAff -> IO Set

lexLtSet :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m Set
lexLtSet = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m Set
  go ma1 ma2 =
    unsafeIslFromIO $ \_ -> c_lexLtSet ma1 ma2


foreign import ccall "isl_multi_aff_get_domain_space" c_getDomainSpace :: MultiAffRef -> IO Space

getDomainSpace :: MonadIO m => MultiAffRef -> IslT m Space
getDomainSpace multi =
    unsafeIslFromIO $ \_ -> c_getDomainSpace multi


foreign import ccall "isl_multi_aff_get_aff" c_getAff :: MultiAffRef -> C.CInt -> IO Aff

getAff :: MonadIO m => MultiAffRef -> Int -> IslT m Aff
getAff multi pos =
    unsafeIslFromIO $ \_ -> c_getAff multi (fromIntegral pos)


foreign import ccall "isl_multi_aff_add_constant_val" c_addConstantVal :: MultiAff -> Val -> IO MultiAff

addConstantVal :: forall m. MonadIO m => MultiAff %1 -> Val %1 -> IslT m MultiAff
addConstantVal = unsafeCoerce go where
  go :: MultiAff -> Val -> IslT m MultiAff
  go mpa v =
    unsafeIslFromIO $ \_ -> c_addConstantVal mpa v


foreign import ccall "isl_multi_aff_add_dims" c_addDims :: MultiAff -> DimType -> C.CUInt -> IO MultiAff

addDims :: forall m. MonadIO m => MultiAff %1 -> DimType -> Int -> IslT m MultiAff
addDims = unsafeCoerce go where
  go :: MultiAff -> DimType -> Int -> IslT m MultiAff
  go multi typ n =
    unsafeIslFromIO $ \_ -> c_addDims multi typ (fromIntegral n)


foreign import ccall "isl_multi_aff_align_params" c_alignParams :: MultiAff -> Space -> IO MultiAff

alignParams :: forall m. MonadIO m => MultiAff %1 -> Space %1 -> IslT m MultiAff
alignParams = unsafeCoerce go where
  go :: MultiAff -> Space -> IslT m MultiAff
  go multi model =
    unsafeIslFromIO $ \_ -> c_alignParams multi model


foreign import ccall "isl_multi_aff_drop_dims" c_dropDims :: MultiAff -> DimType -> C.CUInt -> C.CUInt -> IO MultiAff

dropDims :: forall m. MonadIO m => MultiAff %1 -> DimType -> Int -> Int -> IslT m MultiAff
dropDims = unsafeCoerce go where
  go :: MultiAff -> DimType -> Int -> Int -> IslT m MultiAff
  go multi typ first n =
    unsafeIslFromIO $ \_ -> c_dropDims multi typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_multi_aff_factor_range" c_factorRange :: MultiAff -> IO MultiAff

factorRange :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
factorRange = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_factorRange multi


foreign import ccall "isl_multi_aff_flatten_domain" c_flattenDomain :: MultiAff -> IO MultiAff

flattenDomain :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
flattenDomain = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go ma =
    unsafeIslFromIO $ \_ -> c_flattenDomain ma


foreign import ccall "isl_multi_aff_flatten_range" c_flattenRange :: MultiAff -> IO MultiAff

flattenRange :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
flattenRange = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_flattenRange multi


foreign import ccall "isl_multi_aff_from_range" c_fromRange :: MultiAff -> IO MultiAff

fromRange :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
fromRange = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_fromRange multi


foreign import ccall "isl_multi_aff_identity" c_identity :: Space -> IO MultiAff

identity :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
identity = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_identity space


foreign import ccall "isl_multi_aff_identity_multi_aff" c_identityMultiAff :: MultiAff -> IO MultiAff

identityMultiAff :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
identityMultiAff = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_identityMultiAff multi


foreign import ccall "isl_multi_aff_identity_on_domain_space" c_identityOnDomainSpace :: Space -> IO MultiAff

identityOnDomainSpace :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
identityOnDomainSpace = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_identityOnDomainSpace space


foreign import ccall "isl_multi_aff_insert_dims" c_insertDims :: MultiAff -> DimType -> C.CUInt -> C.CUInt -> IO MultiAff

insertDims :: forall m. MonadIO m => MultiAff %1 -> DimType -> Int -> Int -> IslT m MultiAff
insertDims = unsafeCoerce go where
  go :: MultiAff -> DimType -> Int -> Int -> IslT m MultiAff
  go multi typ first n =
    unsafeIslFromIO $ \_ -> c_insertDims multi typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_multi_aff_move_dims" c_moveDims :: MultiAff -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO MultiAff

moveDims :: forall m. MonadIO m => MultiAff %1 -> DimType -> Int -> DimType -> Int -> Int -> IslT m MultiAff
moveDims = unsafeCoerce go where
  go :: MultiAff -> DimType -> Int -> DimType -> Int -> Int -> IslT m MultiAff
  go ma dst_type dst_pos src_type src_pos n =
    unsafeIslFromIO $ \_ -> c_moveDims ma dst_type (fromIntegral dst_pos) src_type (fromIntegral src_pos) (fromIntegral n)


foreign import ccall "isl_multi_aff_project_domain_on_params" c_projectDomainOnParams :: MultiAff -> IO MultiAff

projectDomainOnParams :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
projectDomainOnParams = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_projectDomainOnParams multi


foreign import ccall "isl_multi_aff_project_out_map" c_projectOutMap :: Space -> DimType -> C.CUInt -> C.CUInt -> IO MultiAff

projectOutMap :: forall m. MonadIO m => Space %1 -> DimType -> Int -> Int -> IslT m MultiAff
projectOutMap = unsafeCoerce go where
  go :: Space -> DimType -> Int -> Int -> IslT m MultiAff
  go space typ first n =
    unsafeIslFromIO $ \_ -> c_projectOutMap space typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_multi_aff_pullback_multi_aff" c_pullbackMultiAff :: MultiAff -> MultiAff -> IO MultiAff

pullbackMultiAff :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m MultiAff
pullbackMultiAff = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m MultiAff
  go ma1 ma2 =
    unsafeIslFromIO $ \_ -> c_pullbackMultiAff ma1 ma2


foreign import ccall "isl_multi_aff_range_factor_domain" c_rangeFactorDomain :: MultiAff -> IO MultiAff

rangeFactorDomain :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
rangeFactorDomain = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_rangeFactorDomain multi


foreign import ccall "isl_multi_aff_range_factor_range" c_rangeFactorRange :: MultiAff -> IO MultiAff

rangeFactorRange :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
rangeFactorRange = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_rangeFactorRange multi


foreign import ccall "isl_multi_aff_range_splice" c_rangeSplice :: MultiAff -> C.CUInt -> MultiAff -> IO MultiAff

rangeSplice :: forall m. MonadIO m => MultiAff %1 -> Int -> MultiAff %1 -> IslT m MultiAff
rangeSplice = unsafeCoerce go where
  go :: MultiAff -> Int -> MultiAff -> IslT m MultiAff
  go multi1 pos multi2 =
    unsafeIslFromIO $ \_ -> c_rangeSplice multi1 (fromIntegral pos) multi2


foreign import ccall "isl_multi_aff_reset_tuple_id" c_resetTupleId :: MultiAff -> DimType -> IO MultiAff

resetTupleId :: forall m. MonadIO m => MultiAff %1 -> DimType -> IslT m MultiAff
resetTupleId = unsafeCoerce go where
  go :: MultiAff -> DimType -> IslT m MultiAff
  go multi typ =
    unsafeIslFromIO $ \_ -> c_resetTupleId multi typ


foreign import ccall "isl_multi_aff_reset_user" c_resetUser :: MultiAff -> IO MultiAff

resetUser :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
resetUser = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_resetUser multi


foreign import ccall "isl_multi_aff_scale_down_val" c_scaleDownVal :: MultiAff -> Val -> IO MultiAff

scaleDownVal :: forall m. MonadIO m => MultiAff %1 -> Val %1 -> IslT m MultiAff
scaleDownVal = unsafeCoerce go where
  go :: MultiAff -> Val -> IslT m MultiAff
  go multi v =
    unsafeIslFromIO $ \_ -> c_scaleDownVal multi v


foreign import ccall "isl_multi_aff_scale_val" c_scaleVal :: MultiAff -> Val -> IO MultiAff

scaleVal :: forall m. MonadIO m => MultiAff %1 -> Val %1 -> IslT m MultiAff
scaleVal = unsafeCoerce go where
  go :: MultiAff -> Val -> IslT m MultiAff
  go multi v =
    unsafeIslFromIO $ \_ -> c_scaleVal multi v


foreign import ccall "isl_multi_aff_set_aff" c_setAff :: MultiAff -> C.CInt -> Aff -> IO MultiAff

setAff :: forall m. MonadIO m => MultiAff %1 -> Int -> Aff %1 -> IslT m MultiAff
setAff = unsafeCoerce go where
  go :: MultiAff -> Int -> Aff -> IslT m MultiAff
  go multi pos el =
    unsafeIslFromIO $ \_ -> c_setAff multi (fromIntegral pos) el


foreign import ccall "isl_multi_aff_set_dim_id" c_setDimId :: MultiAff -> DimType -> C.CUInt -> Id -> IO MultiAff

setDimId :: forall m. MonadIO m => MultiAff %1 -> DimType -> Int -> Id %1 -> IslT m MultiAff
setDimId = unsafeCoerce go where
  go :: MultiAff -> DimType -> Int -> Id -> IslT m MultiAff
  go multi typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId multi typ (fromIntegral pos) id


foreign import ccall "isl_multi_aff_set_dim_name" c_setDimName :: MultiAff -> DimType -> C.CUInt -> C.CString -> IO MultiAff

setDimName :: forall m. MonadIO m => MultiAff %1 -> DimType -> Int -> String -> IslT m MultiAff
setDimName = unsafeCoerce go where
  go :: MultiAff -> DimType -> Int -> String -> IslT m MultiAff
  go multi typ pos s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setDimName multi typ (fromIntegral pos) s_c


foreign import ccall "isl_multi_aff_set_range_tuple_id" c_setRangeTupleId :: MultiAff -> Id -> IO MultiAff

setRangeTupleId :: forall m. MonadIO m => MultiAff %1 -> Id %1 -> IslT m MultiAff
setRangeTupleId = unsafeCoerce go where
  go :: MultiAff -> Id -> IslT m MultiAff
  go multi id =
    unsafeIslFromIO $ \_ -> c_setRangeTupleId multi id


foreign import ccall "isl_multi_aff_set_tuple_id" c_setTupleId :: MultiAff -> DimType -> Id -> IO MultiAff

setTupleId :: forall m. MonadIO m => MultiAff %1 -> DimType -> Id %1 -> IslT m MultiAff
setTupleId = unsafeCoerce go where
  go :: MultiAff -> DimType -> Id -> IslT m MultiAff
  go multi typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId multi typ id


foreign import ccall "isl_multi_aff_set_tuple_name" c_setTupleName :: MultiAff -> DimType -> C.CString -> IO MultiAff

setTupleName :: forall m. MonadIO m => MultiAff %1 -> DimType -> String -> IslT m MultiAff
setTupleName = unsafeCoerce go where
  go :: MultiAff -> DimType -> String -> IslT m MultiAff
  go multi typ s =
    unsafeIslFromIO $ \_ -> do
      s_c <- C.newCString s
      c_setTupleName multi typ s_c


foreign import ccall "isl_multi_aff_splice" c_splice :: MultiAff -> C.CUInt -> C.CUInt -> MultiAff -> IO MultiAff

splice :: forall m. MonadIO m => MultiAff %1 -> Int -> Int -> MultiAff %1 -> IslT m MultiAff
splice = unsafeCoerce go where
  go :: MultiAff -> Int -> Int -> MultiAff -> IslT m MultiAff
  go multi1 in_pos out_pos multi2 =
    unsafeIslFromIO $ \_ -> c_splice multi1 (fromIntegral in_pos) (fromIntegral out_pos) multi2


foreign import ccall "isl_multi_aff_get_dim_id" c_getDimId :: MultiAffRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => MultiAffRef -> DimType -> Int -> IslT m Id
getDimId multi typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId multi typ (fromIntegral pos)


foreign import ccall "isl_multi_aff_get_tuple_id" c_getTupleId :: MultiAffRef -> DimType -> IO Id

getTupleId :: MonadIO m => MultiAffRef -> DimType -> IslT m Id
getTupleId multi typ =
    unsafeIslFromIO $ \_ -> c_getTupleId multi typ


foreign import ccall "isl_multi_aff_to_str" c_toStr :: MultiAffRef -> IO C.CString

toStr :: MultiAffRef -> String
toStr ma =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr ma in r


foreign import ccall "isl_multi_aff_involves_locals" c_involvesLocals :: MultiAffRef -> IO C.CInt

involvesLocals :: MultiAffRef -> Int
involvesLocals multi =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesLocals multi in r


foreign import ccall "isl_multi_aff_involves_nan" c_involvesNan :: MultiAffRef -> IO C.CInt

involvesNan :: MultiAffRef -> Int
involvesNan multi =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesNan multi in r


foreign import ccall "isl_multi_aff_size" c_size :: MultiAffRef -> IO C.CInt

size :: MultiAffRef -> Int
size multi =
    let !r = unsafePerformIO $ fromIntegral <$> c_size multi in r


foreign import ccall "isl_multi_aff_has_range_tuple_id" c_hasRangeTupleId :: MultiAffRef -> IO C.CBool

hasRangeTupleId :: MultiAffRef -> Bool
hasRangeTupleId multi =
    let !r = unsafePerformIO $ M.toBool <$> c_hasRangeTupleId multi in r


foreign import ccall "isl_multi_aff_plain_is_equal" c_plainIsEqual :: MultiAffRef -> MultiAffRef -> IO C.CBool

plainIsEqual :: MultiAffRef -> MultiAffRef -> Bool
plainIsEqual multi1 multi2 =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsEqual multi1 multi2 in r


foreign import ccall "isl_multi_aff_as_set" c_asSet :: MultiAff -> IO Set

asSet :: forall m. MonadIO m => MultiAff %1 -> IslT m Set
asSet = unsafeCoerce go where
  go :: MultiAff -> IslT m Set
  go ma =
    unsafeIslFromIO $ \_ -> c_asSet ma


foreign import ccall "isl_multi_aff_get_space" c_getSpace :: MultiAffRef -> IO Space

getSpace :: MonadIO m => MultiAffRef -> IslT m Space
getSpace multi =
    unsafeIslFromIO $ \_ -> c_getSpace multi


foreign import ccall "isl_multi_aff_as_map" c_asMap :: MultiAff -> IO Map

asMap :: forall m. MonadIO m => MultiAff %1 -> IslT m Map
asMap = unsafeCoerce go where
  go :: MultiAff -> IslT m Map
  go ma =
    unsafeIslFromIO $ \_ -> c_asMap ma


foreign import ccall "isl_multi_aff_get_at" c_getAt :: MultiAffRef -> C.CInt -> IO Aff

getAt :: MonadIO m => MultiAffRef -> Int -> IslT m Aff
getAt multi pos =
    unsafeIslFromIO $ \_ -> c_getAt multi (fromIntegral pos)


foreign import ccall "isl_multi_aff_add" c_add :: MultiAff -> MultiAff -> IO MultiAff

add :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m MultiAff
add = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m MultiAff
  go multi1 multi2 =
    unsafeIslFromIO $ \_ -> c_add multi1 multi2


foreign import ccall "isl_multi_aff_domain_map" c_domainMap :: Space -> IO MultiAff

domainMap :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
domainMap = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_domainMap space


foreign import ccall "isl_multi_aff_domain_reverse" c_domainReverse :: MultiAff -> IO MultiAff

domainReverse :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
domainReverse = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_domainReverse multi


foreign import ccall "isl_multi_aff_flat_range_product" c_flatRangeProduct :: MultiAff -> MultiAff -> IO MultiAff

flatRangeProduct :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m MultiAff
flatRangeProduct = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m MultiAff
  go multi1 multi2 =
    unsafeIslFromIO $ \_ -> c_flatRangeProduct multi1 multi2


foreign import ccall "isl_multi_aff_floor" c_floor :: MultiAff -> IO MultiAff

floor :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
floor = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go ma =
    unsafeIslFromIO $ \_ -> c_floor ma


foreign import ccall "isl_multi_aff_gist" c_gist :: MultiAff -> Set -> IO MultiAff

gist :: forall m. MonadIO m => MultiAff %1 -> Set %1 -> IslT m MultiAff
gist = unsafeCoerce go where
  go :: MultiAff -> Set -> IslT m MultiAff
  go maff context =
    unsafeIslFromIO $ \_ -> c_gist maff context


foreign import ccall "isl_multi_aff_gist_params" c_gistParams :: MultiAff -> Set -> IO MultiAff

gistParams :: forall m. MonadIO m => MultiAff %1 -> Set %1 -> IslT m MultiAff
gistParams = unsafeCoerce go where
  go :: MultiAff -> Set -> IslT m MultiAff
  go maff context =
    unsafeIslFromIO $ \_ -> c_gistParams maff context


foreign import ccall "isl_multi_aff_insert_domain" c_insertDomain :: MultiAff -> Space -> IO MultiAff

insertDomain :: forall m. MonadIO m => MultiAff %1 -> Space %1 -> IslT m MultiAff
insertDomain = unsafeCoerce go where
  go :: MultiAff -> Space -> IslT m MultiAff
  go multi domain =
    unsafeIslFromIO $ \_ -> c_insertDomain multi domain


foreign import ccall "isl_multi_aff_neg" c_neg :: MultiAff -> IO MultiAff

neg :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
neg = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_neg multi


foreign import ccall "isl_multi_aff_product" c_product :: MultiAff -> MultiAff -> IO MultiAff

product :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m MultiAff
product = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m MultiAff
  go multi1 multi2 =
    unsafeIslFromIO $ \_ -> c_product multi1 multi2


foreign import ccall "isl_multi_aff_range_map" c_rangeMap :: Space -> IO MultiAff

rangeMap :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
rangeMap = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_rangeMap space


foreign import ccall "isl_multi_aff_range_product" c_rangeProduct :: MultiAff -> MultiAff -> IO MultiAff

rangeProduct :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m MultiAff
rangeProduct = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m MultiAff
  go multi1 multi2 =
    unsafeIslFromIO $ \_ -> c_rangeProduct multi1 multi2


foreign import ccall "isl_multi_aff_reset_range_tuple_id" c_resetRangeTupleId :: MultiAff -> IO MultiAff

resetRangeTupleId :: forall m. MonadIO m => MultiAff %1 -> IslT m MultiAff
resetRangeTupleId = unsafeCoerce go where
  go :: MultiAff -> IslT m MultiAff
  go multi =
    unsafeIslFromIO $ \_ -> c_resetRangeTupleId multi


foreign import ccall "isl_multi_aff_set_at" c_setAt :: MultiAff -> C.CInt -> Aff -> IO MultiAff

setAt :: forall m. MonadIO m => MultiAff %1 -> Int -> Aff %1 -> IslT m MultiAff
setAt = unsafeCoerce go where
  go :: MultiAff -> Int -> Aff -> IslT m MultiAff
  go multi pos el =
    unsafeIslFromIO $ \_ -> c_setAt multi (fromIntegral pos) el


foreign import ccall "isl_multi_aff_sub" c_sub :: MultiAff -> MultiAff -> IO MultiAff

sub :: forall m. MonadIO m => MultiAff %1 -> MultiAff %1 -> IslT m MultiAff
sub = unsafeCoerce go where
  go :: MultiAff -> MultiAff -> IslT m MultiAff
  go multi1 multi2 =
    unsafeIslFromIO $ \_ -> c_sub multi1 multi2


foreign import ccall "isl_multi_aff_zero" c_zero :: Space -> IO MultiAff

zero :: forall m. MonadIO m => Space %1 -> IslT m MultiAff
zero = unsafeCoerce go where
  go :: Space -> IslT m MultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_zero space


foreign import ccall "isl_multi_aff_to_pw_multi_aff" c_toPwMultiAff :: MultiAff -> IO PwMultiAff

toPwMultiAff :: forall m. MonadIO m => MultiAff %1 -> IslT m PwMultiAff
toPwMultiAff = unsafeCoerce go where
  go :: MultiAff -> IslT m PwMultiAff
  go ma =
    unsafeIslFromIO $ \_ -> c_toPwMultiAff ma


foreign import ccall "isl_multi_aff_get_range_tuple_id" c_getRangeTupleId :: MultiAffRef -> IO Id

getRangeTupleId :: MonadIO m => MultiAffRef -> IslT m Id
getRangeTupleId multi =
    unsafeIslFromIO $ \_ -> c_getRangeTupleId multi


foreign import ccall "isl_multi_aff_get_list" c_getList :: MultiAffRef -> IO AffList

getList :: MonadIO m => MultiAffRef -> IslT m AffList
getList multi =
    unsafeIslFromIO $ \_ -> c_getList multi


foreign import ccall "isl_multi_aff_from_aff" c_fromAff :: Aff -> IO MultiAff

fromAff :: forall m. MonadIO m => Aff %1 -> IslT m MultiAff
fromAff = unsafeCoerce go where
  go :: Aff -> IslT m MultiAff
  go aff =
    unsafeIslFromIO $ \_ -> c_fromAff aff


foreign import ccall "isl_multi_aff_from_aff_list" c_fromAffList :: Space -> AffList -> IO MultiAff

fromAffList :: forall m. MonadIO m => Space %1 -> AffList %1 -> IslT m MultiAff
fromAffList = unsafeCoerce go where
  go :: Space -> AffList -> IslT m MultiAff
  go space list =
    unsafeIslFromIO $ \_ -> c_fromAffList space list


foreign import ccall "isl_multi_aff_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO MultiAff

readFromStr :: MonadIO m => String -> IslT m MultiAff
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_multi_aff_free" c_free :: MultiAff -> IO ()

instance Consumable MultiAff where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_multi_aff_copy" c_copy :: MultiAff -> IO MultiAff

instance Dupable MultiAff where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow MultiAff MultiAffRef where
  borrow = unsafeCoerce $ \(MultiAff ptr) f -> let !r = f (MultiAffRef ptr) in (r, MultiAff ptr)


