{-# LANGUAGE FlexibleInstances #-}

module Array.Representation where

import Control.Exception

class Shape sh where
  dim     :: sh -> Int
  size    :: sh -> Int
  index   :: sh -> sh -> Int
  unindex :: sh -> Int -> sh
  iter    :: sh -> (sh -> a) -> (a -> a -> a) -> a -> a

instance Shape () where
  dim     ()       = 0
  size    ()       = 1
  index   () ()    = 0
  unindex () n     = assert (n == 0) ()
  iter    () f c r = r `c` f ()

instance Shape sh => Shape (sh, Int) where
  dim   (sh, _)          = 1 + dim sh
  size  (sh, sz)         = size sh * sz

  index (sh, sz) (ix, i) = assert (i >= 0 && i < sz)
                         $ index sh ix * sz + i

  unindex (sh, sz) n     = let (d,r) = divMod n sz
			   in  (unindex sh d, r)

  iter (sh, sz) f c r = iter sh (\ix -> iter' (ix,0)) c r
    where
      iter' (ix,i) | i >= sz   = r
                   | otherwise = f (ix,i) `c` iter' (ix,i+1)

