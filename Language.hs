{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language (

  -- Array functions
  use, unit, the, map, zipWith, zipWith3, fold,

  -- Scalar functions
  (!), constant, curry, uncurry, fromIntegral

) where

import Smart
import Array.Sugar      hiding ((!))
import Prelude          hiding (map, zipWith, zipWith3, curry, uncurry, fromIntegral)


-- Array computations
-- ------------------

use :: Arrays arrs => arrs -> Acc arrs
use = Use

unit :: Elt e => Exp e -> Acc (Scalar e)
unit = Unit

the :: Elt e => Acc (Scalar e) -> Exp e
the = (! constant Z)

map :: (Shape sh, Elt a, Elt b)
    => (Exp a -> Exp b) -> Acc (Array sh a) -> Acc (Array sh b)
map = Map

zipWith :: forall sh a b c. (Shape sh, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
        -> Acc (Array sh c)
zipWith f xs ys =
  let arrs :: Acc (Array sh a, Array sh b)
      arrs = arr2 (xs, ys)
  in
  Map (uncurry f) arrs

zipWith3 :: forall sh a b c d. (Shape sh, Elt a, Elt b, Elt c, Elt d)
         => (Exp a -> Exp b -> Exp c -> Exp d)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
zipWith3 f xs ys zs =
  let arrs :: Acc (Array sh a, Array sh b, Array sh c)
      arrs = arr3 (xs, ys, zs)
      f' e = let (x,y,z) = untup3 e in f x y z
  in
  Map f' arrs

fold :: (Shape sh, Elt e)
     => (Exp e -> Exp e -> Exp e)
     -> Exp e
     -> Acc (Array (sh:.Int) e)
     -> Acc (Array sh e)
fold = Fold


-- Scalar functions
-- ----------------

infixl 9 !
(!) :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp sh -> Exp e
(!) = IndexScalar

constant :: Elt e => e -> Exp e
constant = Const

