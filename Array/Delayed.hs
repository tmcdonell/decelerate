{-# LANGUAGE TypeFamilies #-}

module Array.Delayed where

import Array.Sugar

class Delayable a where
  data Delayed a
  delay :: a -> Delayed a
  force :: Delayed a -> a


instance Delayable () where
  data Delayed () = DelayedUnit
  delay ()          = DelayedUnit
  force DelayedUnit = ()

instance Delayable (Array sh e) where
  data Delayed (Array sh e)
    = (Shape sh, Elt e) => DelayedArray { shapeDA :: EltRepr sh
					, repaDA  :: EltRepr sh -> EltRepr e }
  delay arr@(Array sh _)    = DelayedArray sh (fromElt . (arr!) . toElt)
  force (DelayedArray sh f) = newArray (toElt sh) (toElt . f . fromElt)

instance (Delayable a, Delayable b) => Delayable (a, b) where
  data Delayed (a, b) = Delayed2 (Delayed a) (Delayed b)
  delay (a, b)         = Delayed2 (delay a) (delay b)
  force (Delayed2 a b) = (force a, force b)

instance (Delayable a, Delayable b, Delayable c) => Delayable (a, b, c) where
  data Delayed (a, b, c) = Delayed3 (Delayed a) (Delayed b) (Delayed c)
  delay (a, b, c)        = Delayed3 (delay a) (delay b) (delay c)
  force (Delayed3 a b c) = (force a, force b, force c)

