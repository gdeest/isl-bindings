{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}

module Isl.BasicSet.AutoGen where

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

foreign import ccall "isl_basic_set_compare_at" c_compareAt :: BasicSet -> BasicSet -> C.CInt -> IO C.CInt


compareAt :: (Given Ctx) => BasicSet -> BasicSet -> Int -> Int
compareAt = \bset1' bset2' pos' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_compareAt bset1 bset2 pos


foreign import ccall "isl_basic_set_involves_dims" c_involvesDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO C.CInt


involvesDims :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> Int
involvesDims = \bset' typ' first' n' -> 
    unsafePerformIO $ (return . fromIntegral) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_involvesDims bset typ first n


foreign import ccall "isl_basic_set_get_ctx" c_getCtx :: BasicSet -> IO Ctx


getCtx :: (Given Ctx) => BasicSet -> Ctx
getCtx = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_getCtx bset


foreign import ccall "isl_basic_set_dump" c_dump :: BasicSet -> IO ()


dump :: (Given Ctx) => BasicSet -> ()
dump = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_dump bset


foreign import ccall "isl_basic_set_get_dim_name" c_getDimName :: BasicSet -> DimType -> C.CUInt -> IO C.CString


getDimName :: (Given Ctx) => BasicSet -> DimType -> Int -> String
getDimName = \bset' typ' pos' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimName bset typ pos


foreign import ccall "isl_basic_set_get_tuple_name" c_getTupleName :: BasicSet -> IO C.CString


getTupleName :: (Given Ctx) => BasicSet -> String
getTupleName = \bset' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_getTupleName bset


foreign import ccall "isl_basic_set_is_bounded" c_isBounded :: BasicSet -> IO C.CBool


isBounded :: (Given Ctx) => BasicSet -> Bool
isBounded = \bset' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_isBounded bset


foreign import ccall "isl_basic_set_is_disjoint" c_isDisjoint :: BasicSet -> BasicSet -> IO C.CBool


isDisjoint :: (Given Ctx) => BasicSet -> BasicSet -> Bool
isDisjoint = \bset1' bset2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_isDisjoint bset1 bset2


foreign import ccall "isl_basic_set_is_rational" c_isRational :: BasicSet -> IO C.CBool


isRational :: (Given Ctx) => BasicSet -> Bool
isRational = \bset' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_isRational bset


foreign import ccall "isl_basic_set_is_universe" c_isUniverse :: BasicSet -> IO C.CBool


isUniverse :: (Given Ctx) => BasicSet -> Bool
isUniverse = \bset' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_isUniverse bset


foreign import ccall "isl_basic_set_plain_is_empty" c_plainIsEmpty :: BasicSet -> IO C.CBool


plainIsEmpty :: (Given Ctx) => BasicSet -> Bool
plainIsEmpty = \bset' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_plainIsEmpty bset


foreign import ccall "isl_basic_set_plain_is_equal" c_plainIsEqual :: BasicSet -> BasicSet -> IO C.CBool


plainIsEqual :: (Given Ctx) => BasicSet -> BasicSet -> Bool
plainIsEqual = \bset1' bset2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_plainIsEqual bset1 bset2


foreign import ccall "isl_basic_set_plain_is_universe" c_plainIsUniverse :: BasicSet -> IO C.CBool


plainIsUniverse :: (Given Ctx) => BasicSet -> Bool
plainIsUniverse = \bset' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_plainIsUniverse bset


foreign import ccall "isl_basic_set_compute_divs" c_computeDivs :: BasicSet -> IO Set


computeDivs :: (Given Ctx) => BasicSet -> Set
computeDivs = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_computeDivs bset


foreign import ccall "isl_basic_set_get_space" c_getSpace :: BasicSet -> IO Space


getSpace :: (Given Ctx) => BasicSet -> Space
getSpace = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_getSpace bset


foreign import ccall "isl_basic_set_unwrap" c_unwrap :: BasicSet -> IO BasicMap


unwrap :: (Given Ctx) => BasicSet -> BasicMap
unwrap = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_unwrap bset


foreign import ccall "isl_basic_set_add_constraint" c_addConstraint :: BasicSet -> Constraint -> IO BasicSet


addConstraint :: (Given Ctx) => BasicSet -> Constraint -> BasicSet
addConstraint = \bset' constraint' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_addConstraint bset constraint


foreign import ccall "isl_basic_set_add_dims" c_addDims :: BasicSet -> DimType -> C.CUInt -> IO BasicSet


addDims :: (Given Ctx) => BasicSet -> DimType -> Int -> BasicSet
addDims = \bset' typ' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_addDims bset typ n


foreign import ccall "isl_basic_set_align_params" c_alignParams :: BasicSet -> Space -> IO BasicSet


alignParams :: (Given Ctx) => BasicSet -> Space -> BasicSet
alignParams = \bset' model' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      model <- (return) model'

      let ctx = given :: Ctx
      c_alignParams bset model


foreign import ccall "isl_basic_set_coefficients" c_coefficients :: BasicSet -> IO BasicSet


coefficients :: (Given Ctx) => BasicSet -> BasicSet
coefficients = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_coefficients bset


foreign import ccall "isl_basic_set_copy" c_copy :: BasicSet -> IO BasicSet


copy :: (Given Ctx) => BasicSet -> BasicSet
copy = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_copy bset


foreign import ccall "isl_basic_set_drop_constraints_involving_dims" c_dropConstraintsInvolvingDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


dropConstraintsInvolvingDims :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
dropConstraintsInvolvingDims = \bset' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsInvolvingDims bset typ first n


foreign import ccall "isl_basic_set_drop_constraints_not_involving_dims" c_dropConstraintsNotInvolvingDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


dropConstraintsNotInvolvingDims :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
dropConstraintsNotInvolvingDims = \bset' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_dropConstraintsNotInvolvingDims bset typ first n


foreign import ccall "isl_basic_set_drop_unused_params" c_dropUnusedParams :: BasicSet -> IO BasicSet


dropUnusedParams :: (Given Ctx) => BasicSet -> BasicSet
dropUnusedParams = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_dropUnusedParams bset


foreign import ccall "isl_basic_set_eliminate" c_eliminate :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


eliminate :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
eliminate = \bset' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_eliminate bset typ first n


foreign import ccall "isl_basic_set_empty" c_empty :: Space -> IO BasicSet


empty :: (Given Ctx) => Space -> BasicSet
empty = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_empty space


foreign import ccall "isl_basic_set_fix_si" c_fixSi :: BasicSet -> DimType -> C.CUInt -> C.CInt -> IO BasicSet


fixSi :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
fixSi = \bset' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return . fromIntegral) value'

      let ctx = given :: Ctx
      c_fixSi bset typ pos value


foreign import ccall "isl_basic_set_fix_val" c_fixVal :: BasicSet -> DimType -> C.CUInt -> Val -> IO BasicSet


fixVal :: (Given Ctx) => BasicSet -> DimType -> Int -> Val -> BasicSet
fixVal = \bset' typ' pos' v' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      v <- (return) v'

      let ctx = given :: Ctx
      c_fixVal bset typ pos v


foreign import ccall "isl_basic_set_flat_product" c_flatProduct :: BasicSet -> BasicSet -> IO BasicSet


flatProduct :: (Given Ctx) => BasicSet -> BasicSet -> BasicSet
flatProduct = \bset1' bset2' -> 
    unsafePerformIO $ (return) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_flatProduct bset1 bset2


foreign import ccall "isl_basic_set_from_constraint" c_fromConstraint :: Constraint -> IO BasicSet


fromConstraint :: (Given Ctx) => Constraint -> BasicSet
fromConstraint = \constraint' -> 
    unsafePerformIO $ (return) =<< do
      constraint <- (return) constraint'

      let ctx = given :: Ctx
      c_fromConstraint constraint


foreign import ccall "isl_basic_set_from_params" c_fromParams :: BasicSet -> IO BasicSet


fromParams :: (Given Ctx) => BasicSet -> BasicSet
fromParams = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_fromParams bset


foreign import ccall "isl_basic_set_insert_dims" c_insertDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


insertDims :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
insertDims = \bset' typ' pos' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_insertDims bset typ pos n


foreign import ccall "isl_basic_set_lift" c_lift :: BasicSet -> IO BasicSet


lift :: (Given Ctx) => BasicSet -> BasicSet
lift = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_lift bset


foreign import ccall "isl_basic_set_lower_bound_val" c_lowerBoundVal :: BasicSet -> DimType -> C.CUInt -> Val -> IO BasicSet


lowerBoundVal :: (Given Ctx) => BasicSet -> DimType -> Int -> Val -> BasicSet
lowerBoundVal = \bset' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return) value'

      let ctx = given :: Ctx
      c_lowerBoundVal bset typ pos value


foreign import ccall "isl_basic_set_move_dims" c_moveDims :: BasicSet -> DimType -> C.CUInt -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


moveDims :: (Given Ctx) => BasicSet -> DimType -> Int -> DimType -> Int -> Int -> BasicSet
moveDims = \bset' dst_type' dst_pos' src_type' src_pos' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      dst_type <- (return) dst_type'
      dst_pos <- (return . fromIntegral) dst_pos'
      src_type <- (return) src_type'
      src_pos <- (return . fromIntegral) src_pos'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_moveDims bset dst_type dst_pos src_type src_pos n


foreign import ccall "isl_basic_set_nat_universe" c_natUniverse :: Space -> IO BasicSet


natUniverse :: (Given Ctx) => Space -> BasicSet
natUniverse = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_natUniverse space


foreign import ccall "isl_basic_set_neg" c_neg :: BasicSet -> IO BasicSet


neg :: (Given Ctx) => BasicSet -> BasicSet
neg = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_neg bset


foreign import ccall "isl_basic_set_positive_orthant" c_positiveOrthant :: Space -> IO BasicSet


positiveOrthant :: (Given Ctx) => Space -> BasicSet
positiveOrthant = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_positiveOrthant space


foreign import ccall "isl_basic_set_project_out" c_projectOut :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


projectOut :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
projectOut = \bset' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_projectOut bset typ first n


foreign import ccall "isl_basic_set_remove_dims" c_removeDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


removeDims :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
removeDims = \bset' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDims bset typ first n


foreign import ccall "isl_basic_set_remove_divs" c_removeDivs :: BasicSet -> IO BasicSet


removeDivs :: (Given Ctx) => BasicSet -> BasicSet
removeDivs = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_removeDivs bset


foreign import ccall "isl_basic_set_remove_divs_involving_dims" c_removeDivsInvolvingDims :: BasicSet -> DimType -> C.CUInt -> C.CUInt -> IO BasicSet


removeDivsInvolvingDims :: (Given Ctx) => BasicSet -> DimType -> Int -> Int -> BasicSet
removeDivsInvolvingDims = \bset' typ' first' n' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      first <- (return . fromIntegral) first'
      n <- (return . fromIntegral) n'

      let ctx = given :: Ctx
      c_removeDivsInvolvingDims bset typ first n


foreign import ccall "isl_basic_set_remove_redundancies" c_removeRedundancies :: BasicSet -> IO BasicSet


removeRedundancies :: (Given Ctx) => BasicSet -> BasicSet
removeRedundancies = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_removeRedundancies bset


foreign import ccall "isl_basic_set_remove_unknown_divs" c_removeUnknownDivs :: BasicSet -> IO BasicSet


removeUnknownDivs :: (Given Ctx) => BasicSet -> BasicSet
removeUnknownDivs = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_removeUnknownDivs bset


foreign import ccall "isl_basic_set_set_dim_name" c_setDimName :: BasicSet -> DimType -> C.CUInt -> C.CString -> IO BasicSet


setDimName :: (Given Ctx) => BasicSet -> DimType -> Int -> String -> BasicSet
setDimName = \bset' typ' pos' s' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setDimName bset typ pos s


foreign import ccall "isl_basic_set_set_tuple_id" c_setTupleId :: BasicSet -> Id -> IO BasicSet


setTupleId :: (Given Ctx) => BasicSet -> Id -> BasicSet
setTupleId = \bset' id' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      id <- (return) id'

      let ctx = given :: Ctx
      c_setTupleId bset id


foreign import ccall "isl_basic_set_set_tuple_name" c_setTupleName :: BasicSet -> C.CString -> IO BasicSet


setTupleName :: (Given Ctx) => BasicSet -> String -> BasicSet
setTupleName = \set' s' -> 
    unsafePerformIO $ (return) =<< do
      set <- (return) set'
      s <- (C.newCString) s'

      let ctx = given :: Ctx
      c_setTupleName set s


foreign import ccall "isl_basic_set_solutions" c_solutions :: BasicSet -> IO BasicSet


solutions :: (Given Ctx) => BasicSet -> BasicSet
solutions = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_solutions bset


foreign import ccall "isl_basic_set_universe" c_universe :: Space -> IO BasicSet


universe :: (Given Ctx) => Space -> BasicSet
universe = \space' -> 
    unsafePerformIO $ (return) =<< do
      space <- (return) space'

      let ctx = given :: Ctx
      c_universe space


foreign import ccall "isl_basic_set_upper_bound_val" c_upperBoundVal :: BasicSet -> DimType -> C.CUInt -> Val -> IO BasicSet


upperBoundVal :: (Given Ctx) => BasicSet -> DimType -> Int -> Val -> BasicSet
upperBoundVal = \bset' typ' pos' value' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'
      value <- (return) value'

      let ctx = given :: Ctx
      c_upperBoundVal bset typ pos value


foreign import ccall "isl_basic_set_max_val" c_maxVal :: BasicSet -> Aff -> IO Val


maxVal :: (Given Ctx) => BasicSet -> Aff -> Val
maxVal = \bset' obj' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      obj <- (return) obj'

      let ctx = given :: Ctx
      c_maxVal bset obj


foreign import ccall "isl_basic_set_get_div" c_getDiv :: BasicSet -> C.CInt -> IO Aff


getDiv :: (Given Ctx) => BasicSet -> Int -> Aff
getDiv = \bset' pos' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDiv bset pos


foreign import ccall "isl_basic_set_get_dim_id" c_getDimId :: BasicSet -> DimType -> C.CUInt -> IO Id


getDimId :: (Given Ctx) => BasicSet -> DimType -> Int -> Id
getDimId = \bset' typ' pos' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      typ <- (return) typ'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_getDimId bset typ pos


foreign import ccall "isl_basic_set_to_str" c_toStr :: BasicSet -> IO C.CString


toStr :: (Given Ctx) => BasicSet -> String
toStr = \bset' -> 
    unsafePerformIO $ (C.peekCString) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_toStr bset


foreign import ccall "isl_basic_set_get_local_space" c_getLocalSpace :: BasicSet -> IO LocalSpace


getLocalSpace :: (Given Ctx) => BasicSet -> LocalSpace
getLocalSpace = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_getLocalSpace bset


foreign import ccall "isl_basic_set_is_empty" c_isEmpty :: BasicSet -> IO C.CBool


isEmpty :: (Given Ctx) => BasicSet -> Bool
isEmpty = \bset' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_isEmpty bset


foreign import ccall "isl_basic_set_is_equal" c_isEqual :: BasicSet -> BasicSet -> IO C.CBool


isEqual :: (Given Ctx) => BasicSet -> BasicSet -> Bool
isEqual = \bset1' bset2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_isEqual bset1 bset2


foreign import ccall "isl_basic_set_is_subset" c_isSubset :: BasicSet -> BasicSet -> IO C.CBool


isSubset :: (Given Ctx) => BasicSet -> BasicSet -> Bool
isSubset = \bset1' bset2' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_isSubset bset1 bset2


foreign import ccall "isl_basic_set_is_wrapping" c_isWrapping :: BasicSet -> IO C.CBool


isWrapping :: (Given Ctx) => BasicSet -> Bool
isWrapping = \bset' -> 
    unsafePerformIO $ (return . M.toBool) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_isWrapping bset


foreign import ccall "isl_basic_set_lexmax" c_lexmax :: BasicSet -> IO Set


lexmax :: (Given Ctx) => BasicSet -> Set
lexmax = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_lexmax bset


foreign import ccall "isl_basic_set_lexmin" c_lexmin :: BasicSet -> IO Set


lexmin :: (Given Ctx) => BasicSet -> Set
lexmin = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_lexmin bset


foreign import ccall "isl_basic_set_to_set" c_toSet :: BasicSet -> IO Set


toSet :: (Given Ctx) => BasicSet -> Set
toSet = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_toSet bset


foreign import ccall "isl_basic_set_union" c_union :: BasicSet -> BasicSet -> IO Set


union :: (Given Ctx) => BasicSet -> BasicSet -> Set
union = \bset1' bset2' -> 
    unsafePerformIO $ (return) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_union bset1 bset2


foreign import ccall "isl_basic_set_affine_hull" c_affineHull :: BasicSet -> IO BasicSet


affineHull :: (Given Ctx) => BasicSet -> BasicSet
affineHull = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_affineHull bset


foreign import ccall "isl_basic_set_apply" c_apply :: BasicSet -> BasicMap -> IO BasicSet


apply :: (Given Ctx) => BasicSet -> BasicMap -> BasicSet
apply = \bset' bmap' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      bmap <- (return) bmap'

      let ctx = given :: Ctx
      c_apply bset bmap


foreign import ccall "isl_basic_set_detect_equalities" c_detectEqualities :: BasicSet -> IO BasicSet


detectEqualities :: (Given Ctx) => BasicSet -> BasicSet
detectEqualities = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_detectEqualities bset


foreign import ccall "isl_basic_set_flatten" c_flatten :: BasicSet -> IO BasicSet


flatten :: (Given Ctx) => BasicSet -> BasicSet
flatten = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_flatten bset


foreign import ccall "isl_basic_set_gist" c_gist :: BasicSet -> BasicSet -> IO BasicSet


gist :: (Given Ctx) => BasicSet -> BasicSet -> BasicSet
gist = \bset' context' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      context <- (return) context'

      let ctx = given :: Ctx
      c_gist bset context


foreign import ccall "isl_basic_set_intersect" c_intersect :: BasicSet -> BasicSet -> IO BasicSet


intersect :: (Given Ctx) => BasicSet -> BasicSet -> BasicSet
intersect = \bset1' bset2' -> 
    unsafePerformIO $ (return) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_intersect bset1 bset2


foreign import ccall "isl_basic_set_intersect_params" c_intersectParams :: BasicSet -> BasicSet -> IO BasicSet


intersectParams :: (Given Ctx) => BasicSet -> BasicSet -> BasicSet
intersectParams = \bset1' bset2' -> 
    unsafePerformIO $ (return) =<< do
      bset1 <- (return) bset1'
      bset2 <- (return) bset2'

      let ctx = given :: Ctx
      c_intersectParams bset1 bset2


foreign import ccall "isl_basic_set_params" c_params :: BasicSet -> IO BasicSet


params :: (Given Ctx) => BasicSet -> BasicSet
params = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_params bset


foreign import ccall "isl_basic_set_sample" c_sample :: BasicSet -> IO BasicSet


sample :: (Given Ctx) => BasicSet -> BasicSet
sample = \bset' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'

      let ctx = given :: Ctx
      c_sample bset


foreign import ccall "isl_basic_set_dim_max_val" c_dimMaxVal :: BasicSet -> C.CInt -> IO Val


dimMaxVal :: (Given Ctx) => BasicSet -> Int -> Val
dimMaxVal = \bset' pos' -> 
    unsafePerformIO $ (return) =<< do
      bset <- (return) bset'
      pos <- (return . fromIntegral) pos'

      let ctx = given :: Ctx
      c_dimMaxVal bset pos


foreign import ccall "isl_basic_set_read_from_str" c_readFromStr :: Ctx -> C.CString -> IO BasicSet


readFromStr :: (Given Ctx) => String -> BasicSet
readFromStr = \str' -> 
    unsafePerformIO $ (return) =<< do
      str <- (C.newCString) str'

      let ctx = given :: Ctx
      c_readFromStr ctx str


