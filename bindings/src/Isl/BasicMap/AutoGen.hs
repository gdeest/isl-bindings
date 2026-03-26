{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.BasicMap.AutoGen where

import Control.Monad
import Data.Reflection
import Isl.Types

import Foreign.C as C
import Foreign.C.String as C
import Foreign.C.Types as C
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils as M

import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall "isl_basic_map_find_dim_by_name" c_findDimByName :: BasicMap -> DimType -> C.CString -> IO C.CInt


findDimByName :: (Given Ctx) => BasicMap -> DimType -> String -> Int
findDimByName = \bmap' typ' name' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      name <- (C.newCString) name'

      let ctx = given :: Ctx
      c_findDimByName bmap typ name


foreign import ccall "isl_basic_map_involves_dims" c_involvesDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt


involvesDims :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> Int
involvesDims = \bmap' typ' first' n' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_involvesDims bmap typ first n


foreign import ccall "isl_basic_map_get_ctx" c_getCtx :: BasicMap -> IO Ctx


getCtx :: (Given Ctx) => BasicMap -> Ctx
getCtx = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_getCtx bmap


foreign import ccall "isl_basic_map_dump" c_dump :: BasicMap -> IO ()


dump :: (Given Ctx) => BasicMap -> ()
dump = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_dump bmap


foreign import ccall "isl_basic_map_get_dim_name" c_getDimName :: BasicMap -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => BasicMap -> DimType -> Int -> String
getDimName = \bmap' typ' pos' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName bmap typ pos


foreign import ccall "isl_basic_map_get_tuple_name" c_getTupleName :: BasicMap -> DimType -> IO C.CString


getTupleName :: (Given Ctx) => BasicMap -> DimType -> String
getTupleName = \bmap' typ' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'

      let ctx = given :: Ctx
      c_getTupleName bmap typ


foreign import ccall "isl_basic_map_can_curry" c_canCurry :: BasicMap -> IO C.CBool


canCurry :: (Given Ctx) => BasicMap -> Bool
canCurry = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_canCurry bmap


foreign import ccall "isl_basic_map_can_uncurry" c_canUncurry :: BasicMap -> IO C.CBool


canUncurry :: (Given Ctx) => BasicMap -> Bool
canUncurry = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_canUncurry bmap


foreign import ccall "isl_basic_map_can_zip" c_canZip :: BasicMap -> IO C.CBool


canZip :: (Given Ctx) => BasicMap -> Bool
canZip = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_canZip bmap


foreign import ccall "isl_basic_map_has_dim_id" c_hasDimId :: BasicMap -> DimType -> C.CUInt -> IO C.CBool


hasDimId :: (Given Ctx) => BasicMap -> DimType -> Int -> Bool
hasDimId = \bmap' typ' pos' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_hasDimId bmap typ pos


foreign import ccall "isl_basic_map_image_is_bounded" c_imageIsBounded :: BasicMap -> IO C.CBool


imageIsBounded :: (Given Ctx) => BasicMap -> Bool
imageIsBounded = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_imageIsBounded bmap


foreign import ccall "isl_basic_map_is_disjoint" c_isDisjoint :: BasicMap -> BasicMap -> IO C.CBool


isDisjoint :: (Given Ctx) => BasicMap -> BasicMap -> Bool
isDisjoint = \bmap1' bmap2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_isDisjoint bmap1 bmap2


foreign import ccall "isl_basic_map_is_rational" c_isRational :: BasicMap -> IO C.CBool


isRational :: (Given Ctx) => BasicMap -> Bool
isRational = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_isRational bmap


foreign import ccall "isl_basic_map_is_single_valued" c_isSingleValued :: BasicMap -> IO C.CBool


isSingleValued :: (Given Ctx) => BasicMap -> Bool
isSingleValued = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_isSingleValued bmap


foreign import ccall "isl_basic_map_is_strict_subset" c_isStrictSubset :: BasicMap -> BasicMap -> IO C.CBool


isStrictSubset :: (Given Ctx) => BasicMap -> BasicMap -> Bool
isStrictSubset = \bmap1' bmap2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_isStrictSubset bmap1 bmap2


foreign import ccall "isl_basic_map_is_universe" c_isUniverse :: BasicMap -> IO C.CBool


isUniverse :: (Given Ctx) => BasicMap -> Bool
isUniverse = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_isUniverse bmap


foreign import ccall "isl_basic_map_plain_is_empty" c_plainIsEmpty :: BasicMap -> IO C.CBool


plainIsEmpty :: (Given Ctx) => BasicMap -> Bool
plainIsEmpty = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_plainIsEmpty bmap


foreign import ccall "isl_basic_map_plain_is_universe" c_plainIsUniverse :: BasicMap -> IO C.CBool


plainIsUniverse :: (Given Ctx) => BasicMap -> Bool
plainIsUniverse = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_plainIsUniverse bmap


foreign import ccall "isl_basic_map_get_space" c_getSpace :: BasicMap -> IO Space


getSpace :: (Given Ctx) => BasicMap -> Space
getSpace = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_getSpace bmap


foreign import ccall "isl_basic_map_compute_divs" c_computeDivs :: BasicMap -> IO Map


computeDivs :: (Given Ctx) => BasicMap -> Map
computeDivs = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_computeDivs bmap


foreign import ccall "isl_basic_map_add_constraint" c_addConstraint :: BasicMap -> Constraint -> IO BasicMap


addConstraint :: (Given Ctx) => BasicMap -> Constraint -> BasicMap
addConstraint = \bmap' constraint' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_addConstraint bmap constraint


foreign import ccall "isl_basic_map_add_dims" c_addDims :: BasicMap -> DimType -> C.CUInt -> IO BasicMap


addDims :: (Given Ctx) => BasicMap -> DimType -> Int -> BasicMap
addDims = \bmap' typ' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_addDims bmap typ n


foreign import ccall "isl_basic_map_align_params" c_alignParams :: BasicMap -> Space -> IO BasicMap


alignParams :: (Given Ctx) => BasicMap -> Space -> BasicMap
alignParams = \bmap' model' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      model <- (return) model'

      let ctx = given :: Ctx
      c_alignParams bmap model


foreign import ccall "isl_basic_map_copy" c_copy :: BasicMap -> IO BasicMap


copy :: (Given Ctx) => BasicMap -> BasicMap
copy = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_copy bmap


foreign import ccall "isl_basic_map_curry" c_curry :: BasicMap -> IO BasicMap


curry :: (Given Ctx) => BasicMap -> BasicMap
curry = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_curry bmap


foreign import ccall "isl_basic_map_deltas_map" c_deltasMap :: BasicMap -> IO BasicMap


deltasMap :: (Given Ctx) => BasicMap -> BasicMap
deltasMap = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_deltasMap bmap


foreign import ccall "isl_basic_map_domain_map" c_domainMap :: BasicMap -> IO BasicMap


domainMap :: (Given Ctx) => BasicMap -> BasicMap
domainMap = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_domainMap bmap


foreign import ccall "isl_basic_map_domain_product" c_domainProduct :: BasicMap -> BasicMap -> IO BasicMap


domainProduct :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
domainProduct = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_domainProduct bmap1 bmap2


foreign import ccall "isl_basic_map_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


dropConstraintsInvolvingDims :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
dropConstraintsInvolvingDims = \bmap' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsInvolvingDims bmap typ first n


foreign import ccall "isl_basic_map_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


dropConstraintsNotInvolvingDims :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
dropConstraintsNotInvolvingDims = \bmap' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsNotInvolvingDims bmap typ first n


foreign import ccall "isl_basic_map_drop_unused_params" c_dropUnusedParams :: BasicMap -> IO BasicMap


dropUnusedParams :: (Given Ctx) => BasicMap -> BasicMap
dropUnusedParams = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_dropUnusedParams bmap


foreign import ccall "isl_basic_map_eliminate" c_eliminate :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


eliminate :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
eliminate = \bmap' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_eliminate bmap typ first n


foreign import ccall "isl_basic_map_empty" c_empty :: Space -> IO BasicMap


empty :: (Given Ctx) => Space -> BasicMap
empty = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_empty space


foreign import ccall "isl_basic_map_equal" c_equal :: Space -> C.CUInt -> IO BasicMap


equal :: (Given Ctx) => Space -> Int -> BasicMap
equal = \space' n_equal' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      n_equal <- (return . fromIntegral) n_equal'

      let ctx = given :: Ctx
      c_equal space n_equal


foreign import ccall "isl_basic_map_equate" c_equate :: BasicMap -> DimType -> C.CInt -> DimType -> C.CInt -> IO BasicMap


equate :: (Given Ctx) => BasicMap -> DimType -> Int -> DimType -> Int -> BasicMap
equate = \bmap' type1' pos1' type2' pos2' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_equate bmap type1 pos1 type2 pos2


foreign import ccall "isl_basic_map_fix_si" c_fixSi :: BasicMap -> DimType -> C.CUInt -> C.CInt -> IO BasicMap


fixSi :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
fixSi = \bmap' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_fixSi bmap typ pos value


foreign import ccall "isl_basic_map_fix_val" c_fixVal :: BasicMap -> DimType -> C.CUInt -> Val -> IO BasicMap


fixVal :: (Given Ctx) => BasicMap -> DimType -> Int -> Val -> BasicMap
fixVal = \bmap' typ' pos' v' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return) v'

      let ctx = given :: Ctx
      c_fixVal bmap typ pos v


foreign import ccall "isl_basic_map_flat_product" c_flatProduct :: BasicMap -> BasicMap -> IO BasicMap


flatProduct :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
flatProduct = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_flatProduct bmap1 bmap2


foreign import ccall "isl_basic_map_flat_range_product" c_flatRangeProduct :: BasicMap -> BasicMap -> IO BasicMap


flatRangeProduct :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
flatRangeProduct = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_flatRangeProduct bmap1 bmap2


foreign import ccall "isl_basic_map_from_aff" c_fromAff :: Aff -> IO BasicMap


fromAff :: (Given Ctx) => Aff -> BasicMap
fromAff = \aff' -> 
    unsafePerformIO $ (return) =<< do
      aff <- (return) aff'

      let ctx = given :: Ctx
      c_fromAff aff


foreign import ccall "isl_basic_map_from_constraint" c_fromConstraint :: Constraint -> IO BasicMap


fromConstraint :: (Given Ctx) => Constraint -> BasicMap
fromConstraint = \constraint' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_fromConstraint constraint


foreign import ccall "isl_basic_map_from_domain" c_fromDomain :: BasicSet -> IO BasicMap


fromDomain :: (Given Ctx) => BasicSet -> BasicMap
fromDomain = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_fromDomain bset


foreign import ccall "isl_basic_map_from_domain_and_range" c_fromDomainAndRange :: BasicSet -> BasicSet -> IO BasicMap


fromDomainAndRange :: (Given Ctx) => BasicSet -> BasicSet -> BasicMap
fromDomainAndRange = \domain' range' -> 
    unsafePerformIO $ (return) =<< do
      domain <- (return) domain'
      range <- (return) range'

      let ctx = given :: Ctx
      c_fromDomainAndRange domain range


foreign import ccall "isl_basic_map_from_range" c_fromRange :: BasicSet -> IO BasicMap


fromRange :: (Given Ctx) => BasicSet -> BasicMap
fromRange = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_fromRange bset


foreign import ccall "isl_basic_map_gist_domain" c_gistDomain :: BasicMap -> BasicSet -> IO BasicMap


gistDomain :: (Given Ctx) => BasicMap -> BasicSet -> BasicMap
gistDomain = \bmap' context' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gistDomain bmap context


foreign import ccall "isl_basic_map_identity" c_identity :: Space -> IO BasicMap


identity :: (Given Ctx) => Space -> BasicMap
identity = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_identity space


foreign import ccall "isl_basic_map_insert_dims" c_insertDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


insertDims :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
insertDims = \bmap' typ' pos' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_insertDims bmap typ pos n


foreign import ccall "isl_basic_map_less_at" c_lessAt :: Space -> C.CUInt -> IO BasicMap


lessAt :: (Given Ctx) => Space -> Int -> BasicMap
lessAt = \space' pos' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_lessAt space pos


foreign import ccall "isl_basic_map_lower_bound_si" c_lowerBoundSi :: BasicMap -> DimType -> C.CUInt -> C.CInt -> IO BasicMap


lowerBoundSi :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
lowerBoundSi = \bmap' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_lowerBoundSi bmap typ pos value


foreign import ccall "isl_basic_map_more_at" c_moreAt :: Space -> C.CUInt -> IO BasicMap


moreAt :: (Given Ctx) => Space -> Int -> BasicMap
moreAt = \space' pos' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_moreAt space pos


foreign import ccall "isl_basic_map_move_dims" c_moveDims :: BasicMap -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


moveDims :: (Given Ctx) => BasicMap -> DimType -> Int -> DimType -> Int -> Int -> BasicMap
moveDims = \bmap' dst_type' dst_pos' src_type' src_pos' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      dst_type <- (return) dst_type'
      dst_pos <- (return . fromIntegral) dst_pos'
      src_type <- (return) src_type'
      src_pos <- (return . fromIntegral) src_pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_moveDims bmap dst_type dst_pos src_type src_pos n


foreign import ccall "isl_basic_map_nat_universe" c_natUniverse :: Space -> IO BasicMap


natUniverse :: (Given Ctx) => Space -> BasicMap
natUniverse = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_natUniverse space


foreign import ccall "isl_basic_map_neg" c_neg :: BasicMap -> IO BasicMap


neg :: (Given Ctx) => BasicMap -> BasicMap
neg = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_neg bmap


foreign import ccall "isl_basic_map_order_ge" c_orderGe :: BasicMap -> DimType -> C.CInt -> DimType -> C.CInt -> IO BasicMap


orderGe :: (Given Ctx) => BasicMap -> DimType -> Int -> DimType -> Int -> BasicMap
orderGe = \bmap' type1' pos1' type2' pos2' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_orderGe bmap type1 pos1 type2 pos2


foreign import ccall "isl_basic_map_order_gt" c_orderGt :: BasicMap -> DimType -> C.CInt -> DimType -> C.CInt -> IO BasicMap


orderGt :: (Given Ctx) => BasicMap -> DimType -> Int -> DimType -> Int -> BasicMap
orderGt = \bmap' type1' pos1' type2' pos2' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      type1 <- (return) type1'
      pos1 <- (return . fromIntegral) pos1'
      type2 <- (return) type2'
      pos2 <- (return . fromIntegral) pos2'

      let ctx = given :: Ctx
      c_orderGt bmap type1 pos1 type2 pos2


foreign import ccall "isl_basic_map_product" c_product :: BasicMap -> BasicMap -> IO BasicMap


product :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
product = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_product bmap1 bmap2


foreign import ccall "isl_basic_map_project_out" c_projectOut :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


projectOut :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
projectOut = \bmap' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_projectOut bmap typ first n


foreign import ccall "isl_basic_map_range_map" c_rangeMap :: BasicMap -> IO BasicMap


rangeMap :: (Given Ctx) => BasicMap -> BasicMap
rangeMap = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_rangeMap bmap


foreign import ccall "isl_basic_map_range_product" c_rangeProduct :: BasicMap -> BasicMap -> IO BasicMap


rangeProduct :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
rangeProduct = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_rangeProduct bmap1 bmap2


foreign import ccall "isl_basic_map_remove_dims" c_removeDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


removeDims :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
removeDims = \bmap' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDims bmap typ first n


foreign import ccall "isl_basic_map_remove_divs" c_removeDivs :: BasicMap -> IO BasicMap


removeDivs :: (Given Ctx) => BasicMap -> BasicMap
removeDivs = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_removeDivs bmap


foreign import ccall "isl_basic_map_remove_divs_involving_dims" c_removeDivsInvolvingDims :: BasicMap -> DimType -> C.CUInt -> C.CUInt -> IO BasicMap


removeDivsInvolvingDims :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
removeDivsInvolvingDims = \bmap' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDivsInvolvingDims bmap typ first n


foreign import ccall "isl_basic_map_remove_redundancies" c_removeRedundancies :: BasicMap -> IO BasicMap


removeRedundancies :: (Given Ctx) => BasicMap -> BasicMap
removeRedundancies = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_removeRedundancies bmap


foreign import ccall "isl_basic_map_set_dim_name" c_setDimName :: BasicMap -> DimType -> C.CUInt -> C.CString -> IO BasicMap


setDimName :: (Given Ctx) => BasicMap -> DimType -> Int -> String -> BasicMap
setDimName = \bmap' typ' pos' s' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setDimName bmap typ pos s


foreign import ccall "isl_basic_map_set_tuple_id" c_setTupleId :: BasicMap -> DimType -> Id -> IO BasicMap


setTupleId :: (Given Ctx) => BasicMap -> DimType -> Id -> BasicMap
setTupleId = \bmap' typ' id' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setTupleId bmap typ id


foreign import ccall "isl_basic_map_set_tuple_name" c_setTupleName :: BasicMap -> DimType -> C.CString -> IO BasicMap


setTupleName :: (Given Ctx) => BasicMap -> DimType -> String -> BasicMap
setTupleName = \bmap' typ' s' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setTupleName bmap typ s


foreign import ccall "isl_basic_map_sum" c_sum :: BasicMap -> BasicMap -> IO BasicMap


sum :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
sum = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_sum bmap1 bmap2


foreign import ccall "isl_basic_map_uncurry" c_uncurry :: BasicMap -> IO BasicMap


uncurry :: (Given Ctx) => BasicMap -> BasicMap
uncurry = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_uncurry bmap


foreign import ccall "isl_basic_map_universe" c_universe :: Space -> IO BasicMap


universe :: (Given Ctx) => Space -> BasicMap
universe = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_universe space


foreign import ccall "isl_basic_map_upper_bound_si" c_upperBoundSi :: BasicMap -> DimType -> C.CUInt -> C.CInt -> IO BasicMap


upperBoundSi :: (Given Ctx) => BasicMap -> DimType -> Int -> Int -> BasicMap
upperBoundSi = \bmap' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_upperBoundSi bmap typ pos value


foreign import ccall "isl_basic_map_zip" c_zip :: BasicMap -> IO BasicMap


zip :: (Given Ctx) => BasicMap -> BasicMap
zip = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_zip bmap


foreign import ccall "isl_basic_map_domain" c_domain :: BasicMap -> IO BasicSet


domain :: (Given Ctx) => BasicMap -> BasicSet
domain = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_domain bmap


foreign import ccall "isl_basic_map_range" c_range :: BasicMap -> IO BasicSet


range :: (Given Ctx) => BasicMap -> BasicSet
range = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_range bmap


foreign import ccall "isl_basic_map_wrap" c_wrap :: BasicMap -> IO BasicSet


wrap :: (Given Ctx) => BasicMap -> BasicSet
wrap = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_wrap bmap


foreign import ccall "isl_basic_map_plain_get_val_if_fixed" c_plainGetValIfFixed :: BasicMap -> DimType -> C.CUInt -> IO Val


plainGetValIfFixed :: (Given Ctx) => BasicMap -> DimType -> Int -> Val
plainGetValIfFixed = \bmap' typ' pos' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_plainGetValIfFixed bmap typ pos


foreign import ccall "isl_basic_map_get_div" c_getDiv :: BasicMap -> C.CInt -> IO Aff


getDiv :: (Given Ctx) => BasicMap -> Int -> Aff
getDiv = \bmap' pos' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDiv bmap pos


foreign import ccall "isl_basic_map_to_str" c_toStr :: BasicMap -> IO C.CString


toStr :: (Given Ctx) => BasicMap -> String
toStr = \bmap' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_toStr bmap


foreign import ccall "isl_basic_map_get_local_space" c_getLocalSpace :: BasicMap -> IO LocalSpace


getLocalSpace :: (Given Ctx) => BasicMap -> LocalSpace
getLocalSpace = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_getLocalSpace bmap


foreign import ccall "isl_basic_map_is_empty" c_isEmpty :: BasicMap -> IO C.CBool


isEmpty :: (Given Ctx) => BasicMap -> Bool
isEmpty = \bmap' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_isEmpty bmap


foreign import ccall "isl_basic_map_is_equal" c_isEqual :: BasicMap -> BasicMap -> IO C.CBool


isEqual :: (Given Ctx) => BasicMap -> BasicMap -> Bool
isEqual = \bmap1' bmap2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_isEqual bmap1 bmap2


foreign import ccall "isl_basic_map_is_subset" c_isSubset :: BasicMap -> BasicMap -> IO C.CBool


isSubset :: (Given Ctx) => BasicMap -> BasicMap -> Bool
isSubset = \bmap1' bmap2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_isSubset bmap1 bmap2


foreign import ccall "isl_basic_map_lexmax" c_lexmax :: BasicMap -> IO Map


lexmax :: (Given Ctx) => BasicMap -> Map
lexmax = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_lexmax bmap


foreign import ccall "isl_basic_map_lexmin" c_lexmin :: BasicMap -> IO Map


lexmin :: (Given Ctx) => BasicMap -> Map
lexmin = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_lexmin bmap


foreign import ccall "isl_basic_map_union" c_union :: BasicMap -> BasicMap -> IO Map


union :: (Given Ctx) => BasicMap -> BasicMap -> Map
union = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_union bmap1 bmap2


foreign import ccall "isl_basic_map_affine_hull" c_affineHull :: BasicMap -> IO BasicMap


affineHull :: (Given Ctx) => BasicMap -> BasicMap
affineHull = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_affineHull bmap


foreign import ccall "isl_basic_map_apply_domain" c_applyDomain :: BasicMap -> BasicMap -> IO BasicMap


applyDomain :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
applyDomain = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_applyDomain bmap1 bmap2


foreign import ccall "isl_basic_map_apply_range" c_applyRange :: BasicMap -> BasicMap -> IO BasicMap


applyRange :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
applyRange = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_applyRange bmap1 bmap2


foreign import ccall "isl_basic_map_detect_equalities" c_detectEqualities :: BasicMap -> IO BasicMap


detectEqualities :: (Given Ctx) => BasicMap -> BasicMap
detectEqualities = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_detectEqualities bmap


foreign import ccall "isl_basic_map_flatten" c_flatten :: BasicMap -> IO BasicMap


flatten :: (Given Ctx) => BasicMap -> BasicMap
flatten = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_flatten bmap


foreign import ccall "isl_basic_map_flatten_domain" c_flattenDomain :: BasicMap -> IO BasicMap


flattenDomain :: (Given Ctx) => BasicMap -> BasicMap
flattenDomain = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_flattenDomain bmap


foreign import ccall "isl_basic_map_flatten_range" c_flattenRange :: BasicMap -> IO BasicMap


flattenRange :: (Given Ctx) => BasicMap -> BasicMap
flattenRange = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_flattenRange bmap


foreign import ccall "isl_basic_map_gist" c_gist :: BasicMap -> BasicMap -> IO BasicMap


gist :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
gist = \bmap' context' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gist bmap context


foreign import ccall "isl_basic_map_intersect" c_intersect :: BasicMap -> BasicMap -> IO BasicMap


intersect :: (Given Ctx) => BasicMap -> BasicMap -> BasicMap
intersect = \bmap1' bmap2' -> 
    unsafePerformIO $ (return) =<< do
      bmap1 <- (return) bmap1'
      bmap2 <- (return) bmap2'

      let ctx = given :: Ctx
      c_intersect bmap1 bmap2


foreign import ccall "isl_basic_map_intersect_domain" c_intersectDomain :: BasicMap -> BasicSet -> IO BasicMap


intersectDomain :: (Given Ctx) => BasicMap -> BasicSet -> BasicMap
intersectDomain = \bmap' bset' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_intersectDomain bmap bset


foreign import ccall "isl_basic_map_intersect_params" c_intersectParams :: BasicMap -> BasicSet -> IO BasicMap


intersectParams :: (Given Ctx) => BasicMap -> BasicSet -> BasicMap
intersectParams = \bmap' bset' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_intersectParams bmap bset


foreign import ccall "isl_basic_map_intersect_range" c_intersectRange :: BasicMap -> BasicSet -> IO BasicMap


intersectRange :: (Given Ctx) => BasicMap -> BasicSet -> BasicMap
intersectRange = \bmap' bset' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_intersectRange bmap bset


foreign import ccall "isl_basic_map_reverse" c_reverse :: BasicMap -> IO BasicMap


reverse :: (Given Ctx) => BasicMap -> BasicMap
reverse = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_reverse bmap


foreign import ccall "isl_basic_map_sample" c_sample :: BasicMap -> IO BasicMap


sample :: (Given Ctx) => BasicMap -> BasicMap
sample = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_sample bmap


foreign import ccall "isl_basic_map_deltas" c_deltas :: BasicMap -> IO BasicSet


deltas :: (Given Ctx) => BasicMap -> BasicSet
deltas = \bmap' -> 
    unsafePerformIO $ (return) =<< do
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_deltas bmap


foreign import ccall "isl_basic_map_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO BasicMap


readFromStr :: (Given Ctx) => String -> BasicMap
readFromStr = \str' -> 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


