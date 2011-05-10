{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Array.Arrays where

import Type
import Array.Sugar
import Array.Delayed
import Data.Typeable


-- Representation
-- --------------

data ArraysR arrs where
  ArraysRunit  ::                                   ArraysR ()
  ArraysRarray :: (Shape sh, Elt e) =>              ArraysR (Array sh e)
  ArraysRpair  :: ArraysR arrs1 -> ArraysR arrs2 -> ArraysR (arrs1, arrs2)

data ArraysType arrs where
  ArraysTunit  ::                                         ArraysType ()
  ArraysTarray :: Elt e => TupleType (EltRepr e) ->       ArraysType (Array sh e)
  ArraysTpair  :: ArraysType arrs1 -> ArraysType arrs2 -> ArraysType (arrs1, arrs2)

data UniformR sh arrs where
  UniformRunit  ::                                           UniformR sh ()
  UniformRarray :: (Shape sh, Elt e) =>                      UniformR sh (Array sh e)
  UniformRpair  :: UniformR sh arrs1 -> UniformR sh arrs2 -> UniformR sh (arrs1, arrs2)

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
  arrays   :: a {- dummy -} -> ArraysR (ArrRepr  a)
  arrays'  :: a {- dummy -} -> ArraysR (ArrRepr' a)
  --
  toArr    :: ArrRepr  a -> a
  toArr'   :: ArrRepr' a -> a
  fromArr  :: a -> ArrRepr  a
  fromArr' :: a -> ArrRepr' a


instance Arrays () where
  arrays  _ = ArraysRunit
  arrays' _ = ArraysRunit
  --
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


-- Array elements
-- --------------

type family AEltRepr a :: *
type instance AEltRepr ()           = ()
type instance AEltRepr (Array sh e) = EltRepr e
type instance AEltRepr (b, a)       = (AEltRepr b, AEltRepr' a)
type instance AEltRepr (c, b, a)    = (AEltRepr (c, b), AEltRepr' a)

type family AEltRepr' a :: *
type instance AEltRepr' ()           = ()
type instance AEltRepr' (Array sh e) = EltRepr' e
type instance AEltRepr' (b, a)       = (AEltRepr b, AEltRepr' a)
type instance AEltRepr' (c, b, a)    = (AEltRepr (c, b), AEltRepr' a)


class (Arrays arrs, Elt e) => ArraysElt arrs e where
  toAElt  :: arrs -> AEltRepr  (ArrRepr  arrs) -> e
  toAElt' :: arrs -> AEltRepr' (ArrRepr' arrs) -> e

instance ArraysElt () () where
  toAElt  _ = id
  toAElt' _ = id

instance (Shape sh, Elt e) => ArraysElt (Array sh e) e where
  toAElt  _ ((), e) = toElt' e
  toAElt' _ e       = toElt' e

instance ( ArraysElt a2 e2, ArraysElt a1 e1 ) => ArraysElt (a2, a1) (e2, e1) where
  toAElt  _ (a2, a1) = (toAElt (undefined::a2) a2, toAElt' (undefined::a1) a1)
  toAElt' _ (a2, a1) = (toAElt (undefined::a2) a2, toAElt' (undefined::a1) a1)

instance ( ArraysElt a3 e3, ArraysElt a2 e2, ArraysElt a1 e1 )
         => ArraysElt (a3, a2, a1) (e3, e2, e1) where
  toAElt  _ (a32, a1) = let (e3, e2) = toAElt (undefined::(a3, a2)) a32 in (e3, e2, toAElt' (undefined::a1) a1)
  toAElt' _ (a32, a1) = let (e3, e2) = toAElt (undefined::(a3, a2)) a32 in (e3, e2, toAElt' (undefined::a1) a1)

