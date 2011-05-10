{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language (

  -- Array functions
  use, map, zipWith, zipWith3, fold,

  -- Scalar functions
  constant, curry, uncurry

) where

import Smart
import Tuple
import Array.Sugar
import Array.Arrays
import Prelude          hiding (map, zipWith, zipWith3, curry, uncurry)


-- Array computations
-- ------------------

use :: Arrays arrs => arrs -> Acc arrs
use = Use

map :: (Shape sh, Elt a, Elt b)
    => (Exp a -> Exp b) -> Acc (Array sh a) -> Acc (Array sh b)
map = Map arx1

zipWith :: forall sh a b c. (Shape sh, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
        -> Acc (Array sh c)
zipWith f xs ys =
  let arrs :: Acc (Array sh a, Array sh b)
      arrs = Atuple $ NilTup `SnocTup` xs `SnocTup` ys
  in
  Map arx2 (uncurry f) arrs

zipWith3 :: forall sh a b c d. (Shape sh, Elt a, Elt b, Elt c, Elt d)
         => (Exp a -> Exp b -> Exp c -> Exp d)
         -> Acc (Array sh a)
         -> Acc (Array sh b)
         -> Acc (Array sh c)
         -> Acc (Array sh d)
zipWith3 f xs ys zs =
  let arrs :: Acc (Array sh a, Array sh b, Array sh c)
      arrs = Atuple $ NilTup `SnocTup` xs `SnocTup` ys `SnocTup` zs
      f' e = let (x,y,z) = untup3 e in f x y z
  in
  Map arx3 f' arrs


fold :: (Shape sh, Elt e)
     => (Exp e -> Exp e -> Exp e)
     -> Exp e
     -> Acc (Array (sh:.Int) e)
     -> Acc (Array sh e)
fold = Fold arx1


-- Arrays
-- ------

arx1 :: (Shape sh, Elt a) => UniformR sh (ArrRepr (Array sh a))
arx1 = UniformRunit `UniformRpair` UniformRarray

arx2 :: (Shape sh, Elt a, Elt b) => UniformR sh (ArrRepr (Array sh a, Array sh b))
arx2 = arx1 `UniformRpair` UniformRarray

arx3 :: (Shape sh, Elt a, Elt b, Elt c)
     => UniformR sh (ArrRepr (Array sh a, Array sh b, Array sh c))
arx3 = arx2 `UniformRpair` UniformRarray


-- Scalar functions
-- ----------------

constant :: Elt e => e -> Exp e
constant = Const

curry :: (Elt a, Elt b) => (Exp (a, b) -> Exp c) -> Exp a -> Exp b -> Exp c
curry f x y = f $ tup2 (x,y)

uncurry :: (Elt a, Elt b) => (Exp a -> Exp b -> Exp c) -> Exp (a, b) -> Exp c
uncurry f t = let (x, y) = untup2 t in f x y

