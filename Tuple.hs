{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}


module Tuple where

data Tuple c t where
  NilTup  ::                     Tuple c ()
  SnocTup :: Tuple c t -> c s -> Tuple c (t, s)

data TupleIdx t e where
  ZeroTupIdx ::                 TupleIdx (t, s) s
  SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e


class IsTuple t where
  type TupleRepr t
  fromTuple :: t -> TupleRepr t
  toTuple   :: TupleRepr t -> t

instance IsTuple () where
  type TupleRepr () = ()
  fromTuple = id
  toTuple   = id

instance IsTuple (a, b) where
  type TupleRepr (a, b) = (((), a), b)
  fromTuple (x,y)      = (((), x), y)
  toTuple (((), x), y) = (x,y)

instance IsTuple (a, b, c) where
  type TupleRepr (a, b, c)  = (TupleRepr (a, b), c)
  fromTuple (x, y, z)       = ((((), x), y), z)
  toTuple ((((), x), y), z) = (x, y, z)

