{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Array.Sugar where

import Type
import Array.Data
import Data.Typeable
import qualified Array.Representation as Repr
import qualified Data.Vector.Unboxed  as V


-- Shapes
-- ------

data Z = Z
  deriving (Typeable, Show)

infixl 3 :.
data tail :. head = tail :. head
  deriving (Typeable, Show)


class (Elt sh, Repr.Shape (EltRepr sh)) => Shape sh where
  dim     :: sh -> Int
  size    :: sh -> Int
  index   :: sh -> sh -> Int
  unindex :: sh -> Int -> sh
  iter    :: sh -> (sh -> a) -> (a -> a -> a) -> a -> a

  dim           = Repr.dim . fromElt
  size          = Repr.size . fromElt
  index sh ix   = Repr.index (fromElt sh) (fromElt ix)
  unindex sh n  = toElt $ Repr.unindex (fromElt sh) n
  iter sh f c r = Repr.iter (fromElt sh) (f . toElt) c r

instance Shape Z
instance Shape sh => Shape (sh:.Int)


-- Array Elements
-- --------------

type family EltRepr a :: *
type instance EltRepr ()     = ()
type instance EltRepr Z      = ()
type instance EltRepr Int    = ((), Int)
type instance EltRepr Float  = ((), Float)
type instance EltRepr (t:.h)    = (EltRepr t, EltRepr' h)
type instance EltRepr (b, a)    = (EltRepr b, EltRepr' a)
type instance EltRepr (c, b, a) = (EltRepr (c, b), EltRepr' a)

type family EltRepr' a :: *
type instance EltRepr' ()     = ()
type instance EltRepr' Z      = ()
type instance EltRepr' Int    = Int
type instance EltRepr' Float  = Float
type instance EltRepr' (t:.h)    = (EltRepr t, EltRepr' h)
type instance EltRepr' (b, a)    = (EltRepr b, EltRepr' a)
type instance EltRepr' (c, b, a) = (EltRepr (c, b), EltRepr' a)


class ( ArrayElt (EltRepr e), ArrayElt (EltRepr' e)
      , Typeable (EltRepr e), Typeable (EltRepr' e)
      , Typeable e, Show e) => Elt e where
  eltType  :: e -> TupleType (EltRepr e)
  fromElt  :: e -> EltRepr e
  toElt    :: EltRepr e -> e
  --
  eltType' :: e -> TupleType (EltRepr' e)
  fromElt' :: e -> EltRepr' e
  toElt'   :: EltRepr' e -> e


#define mkPrimElt(ty)                                                          \
instance Elt ty where {                                                        \
; eltType  _    = PairTuple UnitTuple (SingleTuple scalarType)                 \
; eltType' _    = SingleTuple scalarType                                       \
; toElt ((), x) = x                                                            \
; fromElt x     = ((), x)                                                      \
; toElt'        = id                                                           \
; fromElt'      = id }

mkPrimElt(Int)
mkPrimElt(Float)

instance Elt () where
  eltType  = const UnitTuple
  eltType' = const UnitTuple
  toElt    = id
  toElt'   = id
  fromElt  = id
  fromElt' = id

instance Elt Z where
  eltType     = const UnitTuple
  eltType'    = const UnitTuple
  toElt    () = Z
  toElt'   () = Z
  fromElt  Z  = ()
  fromElt' Z  = ()

instance (Elt t, Elt h) => Elt (t :. h) where
  toElt    (t, h) = toElt t :. toElt' h
  toElt'   (t, h) = toElt t :. toElt' h
  fromElt  (t:.h) = (fromElt t, fromElt' h)
  fromElt' (t:.h) = (fromElt t, fromElt' h)
  eltType  _      = PairTuple (eltType (undefined::t)) (eltType' (undefined::h))
  eltType' _      = PairTuple (eltType (undefined::t)) (eltType' (undefined::h))

instance (Elt b, Elt a) => Elt (b, a) where
  toElt    (b, a) = (toElt b, toElt' a)
  toElt'   (b, a) = (toElt b, toElt' a)
  fromElt  (b, a) = (fromElt b, fromElt' a)
  fromElt' (b, a) = (fromElt b, fromElt' a)
  eltType  _      = PairTuple (eltType (undefined::b)) (eltType' (undefined::a))
  eltType' _      = PairTuple (eltType (undefined::b)) (eltType' (undefined::a))

instance (Elt c, Elt b, Elt a) => Elt (c, b, a) where
  toElt    (cb, a)   = let (c, b) = toElt cb in (c, b, toElt' a)
  toElt'   (cb, a)   = let (c, b) = toElt cb in (c, b, toElt' a)
  fromElt  (c, b, a) = (fromElt (c, b), fromElt' a)
  fromElt' (c, b, a) = (fromElt (c, b), fromElt' a)
  eltType  _         = PairTuple (eltType (undefined::(c,b))) (eltType' (undefined::a))
  eltType' _         = PairTuple (eltType (undefined::(c,b))) (eltType' (undefined::a))


-- Arrays
-- ------

type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int

type Scalar e = Array DIM0 e
type Vector e = Array DIM1 e

data Array sh e where
  Array :: (Shape sh, Elt e)
        => EltRepr sh
        -> ArrayData (EltRepr e)
        -> Array sh e

deriving instance Typeable2 Array

instance Show (Array sh e) where
  show arr@(Array sh _) =
    "Array (" ++ show (toElt sh :: sh) ++ ") " ++ show (toList arr)

toList :: Elt e => Array sh e -> [e]
toList (Array _ adata) = map toElt (V.toList adata)

fromList :: (Shape sh, Elt e) => sh -> [e] -> Array sh e
fromList sh = Array (fromElt sh) . V.fromList . map fromElt . take (size sh)

infixl 9 !
(!) :: Array sh e -> sh -> e
(!) (Array sh adata) ix = toElt (adata `V.unsafeIndex` index (toElt sh) ix)

newArray :: (Shape sh, Elt e) => sh -> (sh -> e) -> Array sh e
newArray sh f =
  Array (fromElt sh) $ V.generate (size sh) (fromElt . f . unindex sh)


-- Auxiliary
-- ---------

sinkFromElt :: (Elt a, Elt b) => (a -> b) -> (EltRepr a -> EltRepr b)
sinkFromElt f = fromElt . f . toElt

sinkFromElt2 :: (Elt a, Elt b, Elt c) => (a -> b -> c) -> (EltRepr a -> EltRepr b -> EltRepr c)
sinkFromElt2 f = \x y -> fromElt $ f (toElt x) (toElt y)

