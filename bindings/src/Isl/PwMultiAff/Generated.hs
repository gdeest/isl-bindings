{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Isl.PwMultiAff.Generated where

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

foreign import ccall "isl_pw_multi_aff_dim" c_dim :: PwMultiAffRef -> DimType -> IO C.CInt

dim :: PwMultiAffRef -> DimType -> Int
dim pma typ =
    let !r = unsafePerformIO $ fromIntegral <$> c_dim pma typ in r


foreign import ccall "isl_pw_multi_aff_find_dim_by_name" c_findDimByName :: PwMultiAffRef -> DimType -> C.CString -> IO C.CInt

findDimByName :: PwMultiAffRef -> DimType -> String -> Int
findDimByName pma typ name =
    let !r = unsafePerformIO $ do
          name_c <- C.newCString name
          fromIntegral <$> c_findDimByName pma typ name_c
    in r


foreign import ccall "isl_pw_multi_aff_involves_dims" c_involvesDims :: PwMultiAffRef -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt

involvesDims :: PwMultiAffRef -> DimType -> Int -> Int -> Int
involvesDims pma typ first n =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesDims pma typ (fromIntegral first) (fromIntegral n) in r


foreign import ccall "isl_pw_multi_aff_involves_nan" c_involvesNan :: PwMultiAffRef -> IO C.CInt

involvesNan :: PwMultiAffRef -> Int
involvesNan pma =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesNan pma in r


foreign import ccall "isl_pw_multi_aff_involves_param_id" c_involvesParamId :: PwMultiAffRef -> IdRef -> IO C.CInt

involvesParamId :: PwMultiAffRef -> IdRef -> Int
involvesParamId pma id =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesParamId pma id in r


foreign import ccall "isl_pw_multi_aff_dump" c_dump :: PwMultiAffRef -> IO ()

dump :: PwMultiAffRef -> ()
dump pma =
    let !r = unsafePerformIO $ c_dump pma in r


foreign import ccall "isl_pw_multi_aff_get_dim_name" c_getDimName :: PwMultiAffRef -> DimType -> C.CUInt -> IO C.CString

getDimName :: PwMultiAffRef -> DimType -> Int -> String
getDimName pma typ pos =
    let !r = unsafePerformIO $ C.peekCString =<< c_getDimName pma typ (fromIntegral pos) in r


foreign import ccall "isl_pw_multi_aff_get_tuple_name" c_getTupleName :: PwMultiAffRef -> DimType -> IO C.CString

getTupleName :: PwMultiAffRef -> DimType -> String
getTupleName pma typ =
    let !r = unsafePerformIO $ C.peekCString =<< c_getTupleName pma typ in r


foreign import ccall "isl_pw_multi_aff_has_tuple_id" c_hasTupleId :: PwMultiAffRef -> DimType -> IO C.CBool

hasTupleId :: PwMultiAffRef -> DimType -> Bool
hasTupleId pma typ =
    let !r = unsafePerformIO $ M.toBool <$> c_hasTupleId pma typ in r


foreign import ccall "isl_pw_multi_aff_has_tuple_name" c_hasTupleName :: PwMultiAffRef -> DimType -> IO C.CBool

hasTupleName :: PwMultiAffRef -> DimType -> Bool
hasTupleName pma typ =
    let !r = unsafePerformIO $ M.toBool <$> c_hasTupleName pma typ in r


foreign import ccall "isl_pw_multi_aff_is_equal" c_isEqual :: PwMultiAffRef -> PwMultiAffRef -> IO C.CBool

isEqual :: PwMultiAffRef -> PwMultiAffRef -> Bool
isEqual pma1 pma2 =
    let !r = unsafePerformIO $ M.toBool <$> c_isEqual pma1 pma2 in r


foreign import ccall "isl_pw_multi_aff_get_domain_space" c_getDomainSpace :: PwMultiAffRef -> IO Space

getDomainSpace :: MonadIO m => PwMultiAffRef -> IslT m Space
getDomainSpace pma =
    unsafeIslFromIO $ \_ -> c_getDomainSpace pma


foreign import ccall "isl_pw_multi_aff_get_pw_aff" c_getPwAff :: PwMultiAffRef -> C.CInt -> IO PwAff

getPwAff :: MonadIO m => PwMultiAffRef -> Int -> IslT m PwAff
getPwAff pma pos =
    unsafeIslFromIO $ \_ -> c_getPwAff pma (fromIntegral pos)


foreign import ccall "isl_pw_multi_aff_add_constant_val" c_addConstantVal :: PwMultiAff -> Val -> IO PwMultiAff

addConstantVal :: forall m. MonadIO m => PwMultiAff %1 -> Val %1 -> IslT m PwMultiAff
addConstantVal = unsafeCoerce go where
  go :: PwMultiAff -> Val -> IslT m PwMultiAff
  go pma v =
    unsafeIslFromIO $ \_ -> c_addConstantVal pma v


foreign import ccall "isl_pw_multi_aff_align_params" c_alignParams :: PwMultiAff -> Space -> IO PwMultiAff

alignParams :: forall m. MonadIO m => PwMultiAff %1 -> Space %1 -> IslT m PwMultiAff
alignParams = unsafeCoerce go where
  go :: PwMultiAff -> Space -> IslT m PwMultiAff
  go pma model =
    unsafeIslFromIO $ \_ -> c_alignParams pma model


foreign import ccall "isl_pw_multi_aff_alloc" c_alloc :: Set -> MultiAff -> IO PwMultiAff

alloc :: forall m. MonadIO m => Set %1 -> MultiAff %1 -> IslT m PwMultiAff
alloc = unsafeCoerce go where
  go :: Set -> MultiAff -> IslT m PwMultiAff
  go set maff =
    unsafeIslFromIO $ \_ -> c_alloc set maff


foreign import ccall "isl_pw_multi_aff_drop_dims" c_dropDims :: PwMultiAff -> DimType -> C.CUInt -> C.CUInt -> IO PwMultiAff

dropDims :: forall m. MonadIO m => PwMultiAff %1 -> DimType -> Int -> Int -> IslT m PwMultiAff
dropDims = unsafeCoerce go where
  go :: PwMultiAff -> DimType -> Int -> Int -> IslT m PwMultiAff
  go pma typ first n =
    unsafeIslFromIO $ \_ -> c_dropDims pma typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_pw_multi_aff_empty" c_empty :: Space -> IO PwMultiAff

empty :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
empty = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_empty space


foreign import ccall "isl_pw_multi_aff_fix_si" c_fixSi :: PwMultiAff -> DimType -> C.CUInt -> C.CInt -> IO PwMultiAff

fixSi :: forall m. MonadIO m => PwMultiAff %1 -> DimType -> Int -> Int -> IslT m PwMultiAff
fixSi = unsafeCoerce go where
  go :: PwMultiAff -> DimType -> Int -> Int -> IslT m PwMultiAff
  go pma typ pos value =
    unsafeIslFromIO $ \_ -> c_fixSi pma typ (fromIntegral pos) (fromIntegral value)


foreign import ccall "isl_pw_multi_aff_from_domain" c_fromDomain :: Set -> IO PwMultiAff

fromDomain :: forall m. MonadIO m => Set %1 -> IslT m PwMultiAff
fromDomain = unsafeCoerce go where
  go :: Set -> IslT m PwMultiAff
  go set =
    unsafeIslFromIO $ \_ -> c_fromDomain set


foreign import ccall "isl_pw_multi_aff_from_map" c_fromMap :: Map -> IO PwMultiAff

fromMap :: forall m. MonadIO m => Map %1 -> IslT m PwMultiAff
fromMap = unsafeCoerce go where
  go :: Map -> IslT m PwMultiAff
  go map =
    unsafeIslFromIO $ \_ -> c_fromMap map


foreign import ccall "isl_pw_multi_aff_from_set" c_fromSet :: Set -> IO PwMultiAff

fromSet :: forall m. MonadIO m => Set %1 -> IslT m PwMultiAff
fromSet = unsafeCoerce go where
  go :: Set -> IslT m PwMultiAff
  go set =
    unsafeIslFromIO $ \_ -> c_fromSet set


foreign import ccall "isl_pw_multi_aff_identity" c_identity :: Space -> IO PwMultiAff

identity :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
identity = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_identity space


foreign import ccall "isl_pw_multi_aff_identity_on_domain_space" c_identityOnDomainSpace :: Space -> IO PwMultiAff

identityOnDomainSpace :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
identityOnDomainSpace = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_identityOnDomainSpace space


foreign import ccall "isl_pw_multi_aff_intersect_domain_wrapped_domain" c_intersectDomainWrappedDomain :: PwMultiAff -> Set -> IO PwMultiAff

intersectDomainWrappedDomain :: forall m. MonadIO m => PwMultiAff %1 -> Set %1 -> IslT m PwMultiAff
intersectDomainWrappedDomain = unsafeCoerce go where
  go :: PwMultiAff -> Set -> IslT m PwMultiAff
  go pma set =
    unsafeIslFromIO $ \_ -> c_intersectDomainWrappedDomain pma set


foreign import ccall "isl_pw_multi_aff_intersect_domain_wrapped_range" c_intersectDomainWrappedRange :: PwMultiAff -> Set -> IO PwMultiAff

intersectDomainWrappedRange :: forall m. MonadIO m => PwMultiAff %1 -> Set %1 -> IslT m PwMultiAff
intersectDomainWrappedRange = unsafeCoerce go where
  go :: PwMultiAff -> Set -> IslT m PwMultiAff
  go pma set =
    unsafeIslFromIO $ \_ -> c_intersectDomainWrappedRange pma set


foreign import ccall "isl_pw_multi_aff_neg" c_neg :: PwMultiAff -> IO PwMultiAff

neg :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
neg = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_neg pma


foreign import ccall "isl_pw_multi_aff_preimage_domain_wrapped_domain_pw_multi_aff" c_preimageDomainWrappedDomainPwMultiAff :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

preimageDomainWrappedDomainPwMultiAff :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
preimageDomainWrappedDomainPwMultiAff = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_preimageDomainWrappedDomainPwMultiAff pma1 pma2


foreign import ccall "isl_pw_multi_aff_project_domain_on_params" c_projectDomainOnParams :: PwMultiAff -> IO PwMultiAff

projectDomainOnParams :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
projectDomainOnParams = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_projectDomainOnParams pma


foreign import ccall "isl_pw_multi_aff_project_out_map" c_projectOutMap :: Space -> DimType -> C.CUInt -> C.CUInt -> IO PwMultiAff

projectOutMap :: forall m. MonadIO m => Space %1 -> DimType -> Int -> Int -> IslT m PwMultiAff
projectOutMap = unsafeCoerce go where
  go :: Space -> DimType -> Int -> Int -> IslT m PwMultiAff
  go space typ first n =
    unsafeIslFromIO $ \_ -> c_projectOutMap space typ (fromIntegral first) (fromIntegral n)


foreign import ccall "isl_pw_multi_aff_pullback_multi_aff" c_pullbackMultiAff :: PwMultiAff -> MultiAff -> IO PwMultiAff

pullbackMultiAff :: forall m. MonadIO m => PwMultiAff %1 -> MultiAff %1 -> IslT m PwMultiAff
pullbackMultiAff = unsafeCoerce go where
  go :: PwMultiAff -> MultiAff -> IslT m PwMultiAff
  go pma ma =
    unsafeIslFromIO $ \_ -> c_pullbackMultiAff pma ma


foreign import ccall "isl_pw_multi_aff_pullback_pw_multi_aff" c_pullbackPwMultiAff :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

pullbackPwMultiAff :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
pullbackPwMultiAff = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_pullbackPwMultiAff pma1 pma2


foreign import ccall "isl_pw_multi_aff_reset_tuple_id" c_resetTupleId :: PwMultiAff -> DimType -> IO PwMultiAff

resetTupleId :: forall m. MonadIO m => PwMultiAff %1 -> DimType -> IslT m PwMultiAff
resetTupleId = unsafeCoerce go where
  go :: PwMultiAff -> DimType -> IslT m PwMultiAff
  go pma typ =
    unsafeIslFromIO $ \_ -> c_resetTupleId pma typ


foreign import ccall "isl_pw_multi_aff_reset_user" c_resetUser :: PwMultiAff -> IO PwMultiAff

resetUser :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
resetUser = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_resetUser pma


foreign import ccall "isl_pw_multi_aff_scale_down_val" c_scaleDownVal :: PwMultiAff -> Val -> IO PwMultiAff

scaleDownVal :: forall m. MonadIO m => PwMultiAff %1 -> Val %1 -> IslT m PwMultiAff
scaleDownVal = unsafeCoerce go where
  go :: PwMultiAff -> Val -> IslT m PwMultiAff
  go pma v =
    unsafeIslFromIO $ \_ -> c_scaleDownVal pma v


foreign import ccall "isl_pw_multi_aff_scale_val" c_scaleVal :: PwMultiAff -> Val -> IO PwMultiAff

scaleVal :: forall m. MonadIO m => PwMultiAff %1 -> Val %1 -> IslT m PwMultiAff
scaleVal = unsafeCoerce go where
  go :: PwMultiAff -> Val -> IslT m PwMultiAff
  go pma v =
    unsafeIslFromIO $ \_ -> c_scaleVal pma v


foreign import ccall "isl_pw_multi_aff_set_dim_id" c_setDimId :: PwMultiAff -> DimType -> C.CUInt -> Id -> IO PwMultiAff

setDimId :: forall m. MonadIO m => PwMultiAff %1 -> DimType -> Int -> Id %1 -> IslT m PwMultiAff
setDimId = unsafeCoerce go where
  go :: PwMultiAff -> DimType -> Int -> Id -> IslT m PwMultiAff
  go pma typ pos id =
    unsafeIslFromIO $ \_ -> c_setDimId pma typ (fromIntegral pos) id


foreign import ccall "isl_pw_multi_aff_set_pw_aff" c_setPwAff :: PwMultiAff -> C.CUInt -> PwAff -> IO PwMultiAff

setPwAff :: forall m. MonadIO m => PwMultiAff %1 -> Int -> PwAff %1 -> IslT m PwMultiAff
setPwAff = unsafeCoerce go where
  go :: PwMultiAff -> Int -> PwAff -> IslT m PwMultiAff
  go pma pos pa =
    unsafeIslFromIO $ \_ -> c_setPwAff pma (fromIntegral pos) pa


foreign import ccall "isl_pw_multi_aff_set_range_tuple_id" c_setRangeTupleId :: PwMultiAff -> Id -> IO PwMultiAff

setRangeTupleId :: forall m. MonadIO m => PwMultiAff %1 -> Id %1 -> IslT m PwMultiAff
setRangeTupleId = unsafeCoerce go where
  go :: PwMultiAff -> Id -> IslT m PwMultiAff
  go pma id =
    unsafeIslFromIO $ \_ -> c_setRangeTupleId pma id


foreign import ccall "isl_pw_multi_aff_set_tuple_id" c_setTupleId :: PwMultiAff -> DimType -> Id -> IO PwMultiAff

setTupleId :: forall m. MonadIO m => PwMultiAff %1 -> DimType -> Id %1 -> IslT m PwMultiAff
setTupleId = unsafeCoerce go where
  go :: PwMultiAff -> DimType -> Id -> IslT m PwMultiAff
  go pma typ id =
    unsafeIslFromIO $ \_ -> c_setTupleId pma typ id


foreign import ccall "isl_pw_multi_aff_union_lexmax" c_unionLexmax :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

unionLexmax :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
unionLexmax = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_unionLexmax pma1 pma2


foreign import ccall "isl_pw_multi_aff_union_lexmin" c_unionLexmin :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

unionLexmin :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
unionLexmin = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_unionLexmin pma1 pma2


foreign import ccall "isl_pw_multi_aff_get_dim_id" c_getDimId :: PwMultiAffRef -> DimType -> C.CUInt -> IO Id

getDimId :: MonadIO m => PwMultiAffRef -> DimType -> Int -> IslT m Id
getDimId pma typ pos =
    unsafeIslFromIO $ \_ -> c_getDimId pma typ (fromIntegral pos)


foreign import ccall "isl_pw_multi_aff_get_tuple_id" c_getTupleId :: PwMultiAffRef -> DimType -> IO Id

getTupleId :: MonadIO m => PwMultiAffRef -> DimType -> IslT m Id
getTupleId pma typ =
    unsafeIslFromIO $ \_ -> c_getTupleId pma typ


foreign import ccall "isl_pw_multi_aff_to_str" c_toStr :: PwMultiAffRef -> IO C.CString

toStr :: PwMultiAffRef -> String
toStr pma =
    let !r = unsafePerformIO $ C.peekCString =<< c_toStr pma in r


foreign import ccall "isl_pw_multi_aff_involves_locals" c_involvesLocals :: PwMultiAffRef -> IO C.CInt

involvesLocals :: PwMultiAffRef -> Int
involvesLocals pma =
    let !r = unsafePerformIO $ fromIntegral <$> c_involvesLocals pma in r


foreign import ccall "isl_pw_multi_aff_isa_multi_aff" c_isaMultiAff :: PwMultiAffRef -> IO C.CInt

isaMultiAff :: PwMultiAffRef -> Int
isaMultiAff pma =
    let !r = unsafePerformIO $ fromIntegral <$> c_isaMultiAff pma in r


foreign import ccall "isl_pw_multi_aff_n_piece" c_nPiece :: PwMultiAffRef -> IO C.CInt

nPiece :: PwMultiAffRef -> Int
nPiece pma =
    let !r = unsafePerformIO $ fromIntegral <$> c_nPiece pma in r


foreign import ccall "isl_pw_multi_aff_has_range_tuple_id" c_hasRangeTupleId :: PwMultiAffRef -> IO C.CBool

hasRangeTupleId :: PwMultiAffRef -> Bool
hasRangeTupleId pma =
    let !r = unsafePerformIO $ M.toBool <$> c_hasRangeTupleId pma in r


foreign import ccall "isl_pw_multi_aff_plain_is_equal" c_plainIsEqual :: PwMultiAffRef -> PwMultiAffRef -> IO C.CBool

plainIsEqual :: PwMultiAffRef -> PwMultiAffRef -> Bool
plainIsEqual pma1 pma2 =
    let !r = unsafePerformIO $ M.toBool <$> c_plainIsEqual pma1 pma2 in r


foreign import ccall "isl_pw_multi_aff_as_set" c_asSet :: PwMultiAff -> IO Set

asSet :: forall m. MonadIO m => PwMultiAff %1 -> IslT m Set
asSet = unsafeCoerce go where
  go :: PwMultiAff -> IslT m Set
  go pma =
    unsafeIslFromIO $ \_ -> c_asSet pma


foreign import ccall "isl_pw_multi_aff_domain" c_domain :: PwMultiAff -> IO Set

domain :: forall m. MonadIO m => PwMultiAff %1 -> IslT m Set
domain = unsafeCoerce go where
  go :: PwMultiAff -> IslT m Set
  go pma =
    unsafeIslFromIO $ \_ -> c_domain pma


foreign import ccall "isl_pw_multi_aff_get_space" c_getSpace :: PwMultiAffRef -> IO Space

getSpace :: MonadIO m => PwMultiAffRef -> IslT m Space
getSpace pma =
    unsafeIslFromIO $ \_ -> c_getSpace pma


foreign import ccall "isl_pw_multi_aff_as_map" c_asMap :: PwMultiAff -> IO Map

asMap :: forall m. MonadIO m => PwMultiAff %1 -> IslT m Map
asMap = unsafeCoerce go where
  go :: PwMultiAff -> IslT m Map
  go pma =
    unsafeIslFromIO $ \_ -> c_asMap pma


foreign import ccall "isl_pw_multi_aff_get_at" c_getAt :: PwMultiAffRef -> C.CInt -> IO PwAff

getAt :: MonadIO m => PwMultiAffRef -> Int -> IslT m PwAff
getAt pma pos =
    unsafeIslFromIO $ \_ -> c_getAt pma (fromIntegral pos)


foreign import ccall "isl_pw_multi_aff_as_multi_aff" c_asMultiAff :: PwMultiAff -> IO MultiAff

asMultiAff :: forall m. MonadIO m => PwMultiAff %1 -> IslT m MultiAff
asMultiAff = unsafeCoerce go where
  go :: PwMultiAff -> IslT m MultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_asMultiAff pma


foreign import ccall "isl_pw_multi_aff_add" c_add :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

add :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
add = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_add pma1 pma2


foreign import ccall "isl_pw_multi_aff_coalesce" c_coalesce :: PwMultiAff -> IO PwMultiAff

coalesce :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
coalesce = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_coalesce pma


foreign import ccall "isl_pw_multi_aff_domain_map" c_domainMap :: Space -> IO PwMultiAff

domainMap :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
domainMap = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_domainMap space


foreign import ccall "isl_pw_multi_aff_domain_reverse" c_domainReverse :: PwMultiAff -> IO PwMultiAff

domainReverse :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
domainReverse = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_domainReverse pma


foreign import ccall "isl_pw_multi_aff_drop_unused_params" c_dropUnusedParams :: PwMultiAff -> IO PwMultiAff

dropUnusedParams :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
dropUnusedParams = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_dropUnusedParams pma


foreign import ccall "isl_pw_multi_aff_flat_range_product" c_flatRangeProduct :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

flatRangeProduct :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
flatRangeProduct = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_flatRangeProduct pma1 pma2


foreign import ccall "isl_pw_multi_aff_gist" c_gist :: PwMultiAff -> Set -> IO PwMultiAff

gist :: forall m. MonadIO m => PwMultiAff %1 -> Set %1 -> IslT m PwMultiAff
gist = unsafeCoerce go where
  go :: PwMultiAff -> Set -> IslT m PwMultiAff
  go pma set =
    unsafeIslFromIO $ \_ -> c_gist pma set


foreign import ccall "isl_pw_multi_aff_gist_params" c_gistParams :: PwMultiAff -> Set -> IO PwMultiAff

gistParams :: forall m. MonadIO m => PwMultiAff %1 -> Set %1 -> IslT m PwMultiAff
gistParams = unsafeCoerce go where
  go :: PwMultiAff -> Set -> IslT m PwMultiAff
  go pma set =
    unsafeIslFromIO $ \_ -> c_gistParams pma set


foreign import ccall "isl_pw_multi_aff_insert_domain" c_insertDomain :: PwMultiAff -> Space -> IO PwMultiAff

insertDomain :: forall m. MonadIO m => PwMultiAff %1 -> Space %1 -> IslT m PwMultiAff
insertDomain = unsafeCoerce go where
  go :: PwMultiAff -> Space -> IslT m PwMultiAff
  go pma domain =
    unsafeIslFromIO $ \_ -> c_insertDomain pma domain


foreign import ccall "isl_pw_multi_aff_intersect_domain" c_intersectDomain :: PwMultiAff -> Set -> IO PwMultiAff

intersectDomain :: forall m. MonadIO m => PwMultiAff %1 -> Set %1 -> IslT m PwMultiAff
intersectDomain = unsafeCoerce go where
  go :: PwMultiAff -> Set -> IslT m PwMultiAff
  go pma set =
    unsafeIslFromIO $ \_ -> c_intersectDomain pma set


foreign import ccall "isl_pw_multi_aff_intersect_params" c_intersectParams :: PwMultiAff -> Set -> IO PwMultiAff

intersectParams :: forall m. MonadIO m => PwMultiAff %1 -> Set %1 -> IslT m PwMultiAff
intersectParams = unsafeCoerce go where
  go :: PwMultiAff -> Set -> IslT m PwMultiAff
  go pma set =
    unsafeIslFromIO $ \_ -> c_intersectParams pma set


foreign import ccall "isl_pw_multi_aff_product" c_product :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

product :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
product = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_product pma1 pma2


foreign import ccall "isl_pw_multi_aff_range_factor_domain" c_rangeFactorDomain :: PwMultiAff -> IO PwMultiAff

rangeFactorDomain :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
rangeFactorDomain = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_rangeFactorDomain pma


foreign import ccall "isl_pw_multi_aff_range_factor_range" c_rangeFactorRange :: PwMultiAff -> IO PwMultiAff

rangeFactorRange :: forall m. MonadIO m => PwMultiAff %1 -> IslT m PwMultiAff
rangeFactorRange = unsafeCoerce go where
  go :: PwMultiAff -> IslT m PwMultiAff
  go pma =
    unsafeIslFromIO $ \_ -> c_rangeFactorRange pma


foreign import ccall "isl_pw_multi_aff_range_map" c_rangeMap :: Space -> IO PwMultiAff

rangeMap :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
rangeMap = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_rangeMap space


foreign import ccall "isl_pw_multi_aff_range_product" c_rangeProduct :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

rangeProduct :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
rangeProduct = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_rangeProduct pma1 pma2


foreign import ccall "isl_pw_multi_aff_sub" c_sub :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

sub :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
sub = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_sub pma1 pma2


foreign import ccall "isl_pw_multi_aff_subtract_domain" c_subtractDomain :: PwMultiAff -> Set -> IO PwMultiAff

subtractDomain :: forall m. MonadIO m => PwMultiAff %1 -> Set %1 -> IslT m PwMultiAff
subtractDomain = unsafeCoerce go where
  go :: PwMultiAff -> Set -> IslT m PwMultiAff
  go pma set =
    unsafeIslFromIO $ \_ -> c_subtractDomain pma set


foreign import ccall "isl_pw_multi_aff_union_add" c_unionAdd :: PwMultiAff -> PwMultiAff -> IO PwMultiAff

unionAdd :: forall m. MonadIO m => PwMultiAff %1 -> PwMultiAff %1 -> IslT m PwMultiAff
unionAdd = unsafeCoerce go where
  go :: PwMultiAff -> PwMultiAff -> IslT m PwMultiAff
  go pma1 pma2 =
    unsafeIslFromIO $ \_ -> c_unionAdd pma1 pma2


foreign import ccall "isl_pw_multi_aff_zero" c_zero :: Space -> IO PwMultiAff

zero :: forall m. MonadIO m => Space %1 -> IslT m PwMultiAff
zero = unsafeCoerce go where
  go :: Space -> IslT m PwMultiAff
  go space =
    unsafeIslFromIO $ \_ -> c_zero space


foreign import ccall "isl_pw_multi_aff_get_range_tuple_id" c_getRangeTupleId :: PwMultiAffRef -> IO Id

getRangeTupleId :: MonadIO m => PwMultiAffRef -> IslT m Id
getRangeTupleId pma =
    unsafeIslFromIO $ \_ -> c_getRangeTupleId pma


foreign import ccall "isl_pw_multi_aff_from_multi_aff" c_fromMultiAff :: MultiAff -> IO PwMultiAff

fromMultiAff :: forall m. MonadIO m => MultiAff %1 -> IslT m PwMultiAff
fromMultiAff = unsafeCoerce go where
  go :: MultiAff -> IslT m PwMultiAff
  go ma =
    unsafeIslFromIO $ \_ -> c_fromMultiAff ma


foreign import ccall "isl_pw_multi_aff_from_pw_aff" c_fromPwAff :: PwAff -> IO PwMultiAff

fromPwAff :: forall m. MonadIO m => PwAff %1 -> IslT m PwMultiAff
fromPwAff = unsafeCoerce go where
  go :: PwAff -> IslT m PwMultiAff
  go pa =
    unsafeIslFromIO $ \_ -> c_fromPwAff pa


foreign import ccall "isl_pw_multi_aff_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO PwMultiAff

readFromStr :: MonadIO m => String -> IslT m PwMultiAff
readFromStr str =
    unsafeIslFromIO $ \ctx -> do
      str_c <- C.newCString str
      c_readFromStr ctx str_c


foreign import ccall "isl_pw_multi_aff_free" c_free :: PwMultiAff -> IO ()

instance Consumable PwMultiAff where
  consume = unsafeCoerce $ \x -> unsafePerformIO (c_free x)


foreign import ccall "isl_pw_multi_aff_copy" c_copy :: PwMultiAff -> IO PwMultiAff

instance Dupable PwMultiAff where
  dup = unsafeCoerce $ \x -> unsafePerformIO $ do
    copy <- c_copy x
    return (x, copy)


instance Borrow PwMultiAff PwMultiAffRef where
  borrow = unsafeCoerce $ \(PwMultiAff ptr) f -> let !r = f (PwMultiAffRef ptr) in (r, PwMultiAff ptr)


