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
type instance ArrRepr (a, b)       = (ArrRepr a, ArrRepr' b)
type instance ArrRepr (a, b, c)    = (ArrRepr (a, b), ArrRepr' c)

type family ArrRepr' a :: *
type instance ArrRepr' ()           = ()
type instance ArrRepr' (Array sh e) = Array sh e
type instance ArrRepr' (a, b)       = (ArrRepr a, ArrRepr' b)
type instance ArrRepr' (a, b, c)    = (ArrRepr (a, b), ArrRepr' c)


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

instance (Arrays a, Arrays b) => Arrays (a, b) where
  arrays  _ = ArraysRpair (arrays (undefined::a)) (arrays' (undefined::b))
  arrays' _ = ArraysRpair (arrays (undefined::a)) (arrays' (undefined::b))
  --
  toArr  (a, b)   = (toArr a, toArr' b)
  toArr' (a, b)   = (toArr a, toArr' b)
  fromArr  (a, b) = (fromArr a, fromArr' b)
  fromArr' (a, b) = (fromArr a, fromArr' b)

instance (Arrays a, Arrays b, Arrays c) => Arrays (a, b, c) where
  arrays  _ = ArraysRpair (arrays (undefined::(a,b))) (arrays' (undefined::c))
  arrays' _ = ArraysRpair (arrays (undefined::(a,b))) (arrays' (undefined::c))
  --
  toArr  (ab, c)     = let (a, b) = toArr ab in (a, b, toArr' c)
  toArr' (ab, c)     = let (a, b) = toArr ab in (a, b, toArr' c)
  fromArr (a, b, c)  = (fromArr (a, b), fromArr' c)
  fromArr' (a, b, c) = (fromArr (a, b), fromArr' c)

