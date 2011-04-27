{-# LANGUAGE TypeOperators #-}

module Language where

import Smart
import Array.Sugar
import Prelude          hiding (curry, uncurry)


-- Array computations
-- ------------------

use :: Arrays arrs => arrs -> Acc arrs
use = Use

map :: (Shape sh, Elt a, Elt b)
    => (Exp a -> Exp b) -> Acc (Array sh a) -> Acc (Array sh b)
map = Map

zipWith :: (Shape sh, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
        -> Acc (Array sh c)
zipWith f xs ys = Map (uncurry f) (undefined xs ys)
    --(Atuple $ NilTup `SnocTup` xs `SnocTup` ys)


fold :: (Shape sh, Elt e)
     => (Exp e -> Exp e -> Exp e)
     -> Exp e
     -> Acc (Array (sh:.Int) e)
     -> Acc (Array sh e)
fold = Fold


-- Scalar functions
-- ----------------

constant :: Elt e => e -> Exp e
constant = Const

curry :: (Elt a, Elt b) => (Exp (a, b) -> Exp c) -> Exp a -> Exp b -> Exp c
curry f x y = f $ tup2 (x,y)

uncurry :: (Elt a, Elt b) => (Exp a -> Exp b -> Exp c) -> Exp (a, b) -> Exp c
uncurry f t = let (x, y) = untup2 t in f x y

