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

instance (Delayable a1, Delayable a2) => Delayable (a1, a2) where
  data Delayed (a1, a2) = DelayedPair (Delayed a1) (Delayed a2)
  delay (a1, a2)            = DelayedPair (delay a1) (delay a2)
  force (DelayedPair a1 a2) = (force a1, force a2)

