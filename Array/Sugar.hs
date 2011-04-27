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

import Array.Data
import qualified Array.Representation as Repr

import Data.Typeable
import qualified Data.Vector.Unboxed as V


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
type instance EltRepr (a, b)    = (EltRepr a, EltRepr' b)
type instance EltRepr (a, b, c) = (EltRepr (a, b), EltRepr' c)

type family EltRepr' a :: *
type instance EltRepr' ()     = ()
type instance EltRepr' Z      = ()
type instance EltRepr' Int    = Int
type instance EltRepr' Float  = Float
type instance EltRepr' (t:.h)    = (EltRepr t, EltRepr' h)
type instance EltRepr' (a, b)    = (EltRepr a, EltRepr' b)
type instance EltRepr' (a, b, c) = (EltRepr (a, b), EltRepr' c)


class ( ArrayElt (EltRepr e), ArrayElt (EltRepr' e)
      , Typeable (EltRepr e), Typeable (EltRepr' e)
      , Typeable e, Show e) => Elt e where
  toElt    :: EltRepr e -> e
  fromElt  :: e -> EltRepr e
  --
  toElt'   :: EltRepr' e -> e
  fromElt' :: e -> EltRepr' e


#define mkPrimElt(ty)                                                          \
instance Elt ty where {                                                        \
; toElt ((), x) = x                                                            \
; fromElt x     = ((), x)                                                      \
; toElt'        = id                                                           \
; fromElt'      = id }

mkPrimElt(Int)
mkPrimElt(Float)

instance Elt () where
  toElt    = id
  toElt'   = id
  fromElt  = id
  fromElt' = id

instance Elt Z where
  toElt    () = Z
  toElt'   () = Z
  fromElt  Z  = ()
  fromElt' Z  = ()

instance (Elt t, Elt h) => Elt (t :. h) where
  toElt    (t, h) = toElt t :. toElt' h
  toElt'   (t, h) = toElt t :. toElt' h
  fromElt  (t:.h) = (fromElt t, fromElt' h)
  fromElt' (t:.h) = (fromElt t, fromElt' h)

instance (Elt a, Elt b) => Elt (a, b) where
  toElt   (a, b)  = (toElt a, toElt' b)
  toElt'  (a, b)  = (toElt a, toElt' b)
  fromElt  (a, b) = (fromElt a, fromElt' b)
  fromElt' (a, b) = (fromElt a, fromElt' b)

instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
  toElt  (ab, c)     = let (a, b) = toElt ab in (a, b, toElt' c)
  toElt' (ab, c)     = let (a, b) = toElt ab in (a, b, toElt' c)
  fromElt (a, b, c)  = (fromElt (a, b), fromElt' c)
  fromElt' (a, b, c) = (fromElt (a, b), fromElt' c)


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

--  UnitArray ::                             Array sh ()
--  SnocArray :: Array sh a -> Array sh b -> Array sh (a, b)

deriving instance Typeable2 Array

instance Show (Array sh e) where
  show arr@(Array sh _) =
    "Array " ++ show (toElt sh :: sh) ++ " " ++ show (toList arr)

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

sinkFromElt :: (Elt a, Elt b)
            => (a -> b) -> (EltRepr a -> EltRepr b)
sinkFromElt f = fromElt . f . toElt

sinkFromElt2 :: (Elt a, Elt b, Elt c)
             => (a -> b -> c) -> (EltRepr a -> EltRepr b -> EltRepr c)
sinkFromElt2 f = \x y -> fromElt $ f (toElt x) (toElt y)

