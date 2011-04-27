
module Array.Data where

import qualified Data.Vector.Unboxed as V

type ArrayData e = V.Vector e

class V.Unbox e => ArrayElt e

instance ArrayElt ()
instance ArrayElt Int
instance ArrayElt Float

instance (ArrayElt a, ArrayElt b)             => ArrayElt (a, b)
instance (ArrayElt a, ArrayElt b, ArrayElt c) => ArrayElt (a, b, c)

