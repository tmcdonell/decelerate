{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

-- TLM: Surely there is a cleaner way to do this?
--

data UniformR sh arrs e where
  UniformRunit  ::                                                 UniformR sh () ()
  UniformRarray :: (Shape sh, Elt e) =>                            UniformR sh (Array sh e) e
  UniformRpair  :: UniformR sh arrs1 e1 -> UniformR sh arrs2 e2 -> UniformR sh (arrs1, arrs2) (e1, e2)

type family AEltRepr a :: *
type instance AEltRepr ()           = ()
type instance AEltRepr (Array sh e) = ((), e)
type instance AEltRepr (b, a)       = (AEltRepr b, AEltRepr' a)
type instance AEltRepr (c, b, a)    = (AEltRepr (c, b), AEltRepr' a)

type family AEltRepr' a :: *
type instance AEltRepr' ()           = ()
type instance AEltRepr' (Array sh e) = e
type instance AEltRepr' (b, a)       = (AEltRepr b, AEltRepr' a)
type instance AEltRepr' (c, b, a)    = (AEltRepr (c, b), AEltRepr' a)


class (Arrays a, Shape sh, Elt e) => UniformArrays sh a e where
  uniform  :: sh {- dummy -} -> a {- dummy -} -> e {- dummy -} -> UniformR sh (ArrRepr  a) (AEltRepr  (ArrRepr  a))
  uniform' :: sh {- dummy -} -> a {- dummy -} -> e {- dummy -} -> UniformR sh (ArrRepr' a) (AEltRepr' (ArrRepr' a))

  toAElt   :: sh {- dummy -} -> a {- dummy -} -> AEltRepr  (ArrRepr  a) -> e
  toAElt'  :: sh {- dummy -} -> a {- dummy -} -> AEltRepr' (ArrRepr' a) -> e

instance Shape sh => UniformArrays sh () () where
  uniform  _ _ _ = UniformRunit
  uniform' _ _ _ = UniformRunit
  --
  toAElt  _ _ = id
  toAElt' _ _ = id

instance (Shape sh, Elt e) => UniformArrays sh (Array sh e) e where
  uniform  _ _ _ = UniformRpair UniformRunit UniformRarray
  uniform' _ _ _ = UniformRarray
  --
  toAElt  _ _ ((), e) = e
  toAElt' _ _ e       = e

instance (UniformArrays sh a2 e2, UniformArrays sh a1 e1)
         => UniformArrays sh (a2, a1) (e2, e1) where
  uniform  _ _ _ = UniformRpair (uniform (undefined::sh) (undefined::a2) (undefined::e2)) (uniform' (undefined::sh) (undefined::a1) (undefined::e1))
  uniform' _ _ _ = UniformRpair (uniform (undefined::sh) (undefined::a2) (undefined::e2)) (uniform' (undefined::sh) (undefined::a1) (undefined::e1))
  --
  toAElt  _ _ (a2, a1) = (toAElt (undefined::sh) (undefined::a2) a2, toAElt' (undefined::sh) (undefined::a1) a1)
  toAElt' _ _ (a2, a1) = (toAElt (undefined::sh) (undefined::a2) a2, toAElt' (undefined::sh) (undefined::a1) a1)


instance (UniformArrays sh a3 e3, UniformArrays sh a2 e2, UniformArrays sh a1 e1)
         => UniformArrays sh (a3, a2, a1) (e3, e2, e1) where
  uniform  _ _ _ = UniformRpair (uniform (undefined::sh) (undefined::(a3,a2)) (undefined::(e3,e2))) (uniform' (undefined::sh) (undefined::a1) (undefined::e1))
  uniform' _ _ _ = UniformRpair (uniform (undefined::sh) (undefined::(a3,a2)) (undefined::(e3,e2))) (uniform' (undefined::sh) (undefined::a1) (undefined::e1))
  --
  toAElt  _ _ (a32, a1) = let (e3, e2) = toAElt (undefined::sh) (undefined::(a3, a2)) a32 in (e3, e2, toAElt' (undefined::sh) (undefined::a1) a1)
  toAElt' _ _ (a32, a1) = let (e3, e2) = toAElt (undefined::sh) (undefined::(a3, a2)) a32 in (e3, e2, toAElt' (undefined::sh) (undefined::a1) a1)

