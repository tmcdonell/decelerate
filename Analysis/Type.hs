{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.Type where

import Type
import Array.Sugar
import Array.Arrays


data ArraysType arrs where
  ArraysTunit  ::                                         ArraysType ()
  ArraysTarray :: Elt e => TupleType (EltRepr e) ->       ArraysType (Array sh e)
  ArraysTpair  :: ArraysType arrs1 -> ArraysType arrs2 -> ArraysType (arrs1, arrs2)


arrayType :: forall sh e. Array sh e -> TupleType (EltRepr e)
arrayType (Array _ _) = eltType (undefined::e)


arraysType :: forall arrs. Arrays arrs => arrs -> ArraysType (ArrRepr arrs)
arraysType = collect (arrays (undefined :: arrs)) . fromArr
  where
    collect :: ArraysR a -> a -> ArraysType a
    collect ArraysRunit         ()       = ArraysTunit
    collect ArraysRarray        arr      = ArraysTarray (arrayType arr)
    collect (ArraysRpair r1 r2) (a1, a2) = ArraysTpair (collect r1 a1) (collect r2 a2)

