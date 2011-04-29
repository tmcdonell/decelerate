{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Array.Arrays where

import Array.Sugar
import Array.Delayed
import Data.Typeable


-- Representation
-- --------------

data ArraysR arrs where
  ArraysRunit  ::                                   ArraysR ()
  ArraysRarray :: (Shape sh, Elt e) =>              ArraysR (Array sh e)
  ArraysRpair  :: ArraysR arrs1 -> ArraysR arrs2 -> ArraysR (arrs1, arrs2)

type family ArrRepr a :: *
type instance ArrRepr ()           = ()
type instance ArrRepr (Array sh e) = ((), Array sh e)
type instance ArrRepr (b, a)       = (ArrRepr b, ArrRepr' a)
type instance ArrRepr (c, b, a)    = (ArrRepr (c, b), ArrRepr' a)

type family ArrRepr' a :: *
type instance ArrRepr' ()           = ()
type instance ArrRepr' (Array sh e) = Array sh e
type instance ArrRepr' (b, a)       = (ArrRepr b, ArrRepr' a)
type instance ArrRepr' (c, b, a)    = (ArrRepr (c, b), ArrRepr' a)


-- Arrays
-- ------

class ( Typeable (ArrRepr a), Typeable (ArrRepr' a)
      , Delayable a, Typeable a ) => Arrays a where
  arrays   :: a -> ArraysR (ArrRepr  a)
  arrays'  :: a -> ArraysR (ArrRepr' a)
  --
  toArr    :: ArrRepr  a -> a
  toArr'   :: ArrRepr' a -> a
  fromArr  :: a -> ArrRepr  a
  fromArr' :: a -> ArrRepr' a


instance Arrays () where
  arrays  _ = ArraysRunit
  arrays' _ = ArraysRunit
  toArr     = id
  toArr'    = id
  fromArr   = id
  fromArr'  = id

instance (Shape sh, Elt e) => Arrays (Array sh e) where
  arrays  _       = ArraysRpair ArraysRunit ArraysRarray
  arrays' _       = ArraysRarray
  --
  toArr ((), arr) = arr
  toArr'          = id
  fromArr arr     = ((), arr)
  fromArr'        = id

instance (Arrays b, Arrays a) => Arrays (b, a) where
  arrays  _ = ArraysRpair (arrays (undefined::b)) (arrays' (undefined::a))
  arrays' _ = ArraysRpair (arrays (undefined::b)) (arrays' (undefined::a))
  --
  toArr    (b, a) = (toArr b, toArr' a)
  toArr'   (b, a) = (toArr b, toArr' a)
  fromArr  (b, a) = (fromArr b, fromArr' a)
  fromArr' (b, a) = (fromArr b, fromArr' a)

instance (Arrays c, Arrays b, Arrays a) => Arrays (c, b, a) where
  arrays  _ = ArraysRpair (arrays (undefined::(c,b))) (arrays' (undefined::a))
  arrays' _ = ArraysRpair (arrays (undefined::(c,b))) (arrays' (undefined::a))
  --
  toArr    (cb, a)   = let (c, b) = toArr cb in (c, b, toArr' a)
  toArr'   (cb, a)   = let (c, b) = toArr cb in (c, b, toArr' a)
  fromArr  (c, b, a) = (fromArr (c, b), fromArr' a)
  fromArr' (c, b, a) = (fromArr (c, b), fromArr' a)

