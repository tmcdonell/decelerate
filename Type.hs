{-# LANGUAGE GADTs #-}

module Type where

import Data.Bits
import Foreign.Storable


-- Reified dictionaries
-- --------------------

data IntegralDict a where
  IntegralDict :: ( Bounded a, Enum a, Eq a, Ord a, Show a
                  , Bits a, Integral a, Num a, Real a, Storable a)
               => IntegralDict a

data FloatingDict a where
  FloatingDict :: ( Enum a, Eq a, Ord a, Show a
                  , Floating a, Fractional a, Num a, Real a, RealFrac a
                  , RealFloat a, Storable a)
               => FloatingDict a


integralDict :: IntegralType a -> IntegralDict a
integralDict (TypeInt dict) = dict

floatingDict :: FloatingType a -> FloatingDict a
floatingDict (TypeFloat dict) = dict


-- Scalar types
-- ------------

data IntegralType a where
  TypeInt :: IntegralDict Int -> IntegralType Int

data FloatingType a where
  TypeFloat :: FloatingDict Float -> FloatingType Float

data NumType a where
  IntegralNumType :: IntegralType a -> NumType a
  FloatingNumType :: FloatingType a -> NumType a

data ScalarType a where
  NumScalarType :: NumType a -> ScalarType a


-- Querying scalar type representations
--

class (IsNum a, IsScalar a, Integral a) => IsIntegral a where
  integralType :: IntegralType a

instance IsIntegral Int where
  integralType = TypeInt IntegralDict


class (IsNum a, IsScalar a, Floating a) => IsFloating a where
  floatingType :: FloatingType a

instance IsFloating Float where
  floatingType = TypeFloat FloatingDict


class (IsScalar a, Num a) => IsNum a where
  numType :: NumType a

instance IsNum Int where
  numType = IntegralNumType integralType

instance IsNum Float where
  numType = FloatingNumType floatingType


class IsScalar a where
  scalarType :: ScalarType a

instance IsScalar Int where
  scalarType = NumScalarType numType

instance IsScalar Float where
  scalarType = NumScalarType numType


-- Tuple types
-- -----------

data TupleType a where
  UnitTuple   ::                               TupleType ()
  SingleTuple :: ScalarType a               -> TupleType a
  PairTuple   :: TupleType a -> TupleType b -> TupleType (a, b)

