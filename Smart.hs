{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Smart (
  Arrays(..),
  module Smart
) where

import Type
import Tuple
import Pretty                   ()
import Array.Sugar
import Array.Arrays
import qualified AST

import Prelude                  hiding (curry, uncurry)
import Data.Typeable


data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t
              => Layout env env' -> AST.Idx env t -> Layout env (env', t)

prjLayout :: Typeable t => Int -> Layout env env' -> AST.Idx env t
prjLayout  0 (PushLayout _   ix) | Just ix' <- gcast ix = ix'
prjLayout !n (PushLayout lyt _)                         = prjLayout (n-1) lyt
prjLayout  _ _                                          = error "prjLayout: inconsistent valuation"


-- Array computations
-- ------------------

data Acc a where
  Atag          :: Arrays arrs
                => Int
                -> Acc arrs

  Aprj          :: Arrays arrs
                => TupleIdx (ArrRepr arrs) a
                -> Acc arrs
                -> Acc a

  Atuple        :: Arrays arrs
                => Tuple Acc (ArrRepr arrs)
                -> Acc arrs

  Use           :: Arrays arrs
                => arrs
                -> Acc arrs

  Unit          :: Elt e
                => Exp e
                -> Acc (Scalar e)

  Map           :: (UniformArrays sh arrs a, Elt b)
                => (Exp a -> Exp b)
                -> Acc arrs
                -> Acc (Array sh b)

  Fold          :: (UniformArrays (sh:.Int) arrs e, Shape sh)
                => (Exp e -> Exp e -> Exp e)
                -> Exp e
                -> Acc arrs
                -> Acc (Array sh e)


-- Scalar expressions
-- ------------------

data Exp e where
  Tag           :: Elt e => Int -> Exp e
  Const         :: Elt e => e   -> Exp e
  Prj           :: (Elt t, IsTuple t)
                => TupleIdx (TupleRepr t) e -> Exp t -> Exp e
  Tuple         :: (Elt t, IsTuple t)
                => Tuple.Tuple Exp (TupleRepr t) -> Exp t
  PrimApp       :: (Elt a, Elt r)
                => AST.PrimFun (a -> r) -> Exp a -> Exp r
  IndexScalar   :: (Shape sh, Elt e)
                => Acc (Array sh e) -> Exp sh -> Exp e


-- HOAS to de Bruijn conversion
-- ----------------------------

convertAcc :: Arrays a => Acc a -> AST.Acc a
convertAcc = convertOpenAcc EmptyLayout

convertExp :: Layout aenv aenv -> Exp e -> AST.Exp aenv e
convertExp = convertOpenExp EmptyLayout


convertOpenAcc :: Layout aenv aenv
               -> Acc a
               -> AST.OpenAcc aenv a
convertOpenAcc alyt acc =
  case acc of
    Atag n        -> AST.Avar (prjLayout n alyt)
    Aprj ix a     -> AST.Aprj ix (convertOpenAcc alyt a)
    Atuple tup    -> AST.Atuple (convertAtuple alyt tup)
    Use arr       -> AST.Use (fromArr arr)
    Unit e        -> AST.Unit (convertExp alyt e)
    Map f a       -> AST.Map (convertFun1 alyt f) (convertOpenAcc alyt a)
    Fold f e a    -> AST.Fold (convertFun2 alyt f) (convertExp alyt e) (convertOpenAcc alyt a)


convertOpenExp :: forall env aenv e.
                  Layout env  env
               -> Layout aenv aenv
               -> Exp e
               -> AST.OpenExp env aenv e
convertOpenExp lyt alyt = cvt
  where
    cvt :: Exp t -> AST.OpenExp env aenv t
    cvt (Tag n)            = AST.Var (prjLayout n lyt)
    cvt (Const c)          = AST.Const (fromElt c)
    cvt (Prj ix t)         = AST.Prj ix (cvt t)
    cvt (Tuple tup)        = AST.Tuple (convertTuple lyt alyt tup)
    cvt (PrimApp f a)      = AST.PrimApp f (cvt a)
    cvt (IndexScalar a ix) = AST.IndexScalar (convertOpenAcc alyt a) (cvt ix)


convertAtuple :: forall aenv t.
                 Layout aenv aenv
              -> Tuple.Tuple Acc t
              -> Tuple.Tuple (AST.OpenAcc aenv) t
convertAtuple alyt = cvt
  where
    cvt :: Tuple.Tuple Acc tup -> Tuple.Tuple (AST.OpenAcc aenv) tup
    cvt NilTup         = NilTup
    cvt (SnocTup as a) = cvt as `SnocTup` convertOpenAcc alyt a


convertTuple :: forall env aenv t.
                Layout env  env
             -> Layout aenv aenv
             -> Tuple.Tuple Exp t
             -> Tuple.Tuple (AST.OpenExp env aenv) t
convertTuple lyt alyt = cvt
  where
    cvt :: Tuple.Tuple Exp tup -> Tuple.Tuple (AST.OpenExp env aenv) tup
    cvt NilTup         = NilTup
    cvt (SnocTup es e) = cvt es `SnocTup` convertOpenExp lyt alyt e


convertFun1 :: Elt a
            => Layout aenv aenv
            -> (Exp a -> Exp b)
            -> AST.Fun aenv (a -> b)
convertFun1 alyt f = AST.Lam (AST.Body openF)
  where
    a     = Tag 0
    lyt   = EmptyLayout `PushLayout` AST.ZeroIdx
    openF = convertOpenExp lyt alyt (f a)

convertFun2 :: (Elt a, Elt b)
            => Layout aenv aenv
            -> (Exp a -> Exp b -> Exp c)
            -> AST.Fun aenv (a -> b -> c)
convertFun2 alyt f = AST.Lam (AST.Lam (AST.Body openF))
  where
    a     = Tag 1
    b     = Tag 0
    lyt   = EmptyLayout `PushLayout` AST.SuccIdx AST.ZeroIdx
                        `PushLayout` AST.ZeroIdx
    openF = convertOpenExp lyt alyt (f a b)


-- Tuples
-- ------

curry :: (Elt a, Elt b) => (Exp (a, b) -> Exp c) -> Exp a -> Exp b -> Exp c
curry f x y = f $ tup2 (x,y)

uncurry :: (Elt a, Elt b) => (Exp a -> Exp b -> Exp c) -> Exp (a, b) -> Exp c
uncurry f t = let (x, y) = untup2 t in f x y


tup2 :: (Elt a, Elt b) => (Exp a, Exp b) -> Exp (a, b)
tup2 (x, y) = Tuple $ NilTup `SnocTup` x `SnocTup` y

tup3 :: (Elt a, Elt b, Elt c) => (Exp a, Exp b, Exp c) -> Exp (a, b, c)
tup3 (x, y, z) = Tuple $ NilTup `SnocTup` x `SnocTup` y `SnocTup` z

untup2 :: (Elt a, Elt b) => Exp (a, b) -> (Exp a, Exp b)
untup2 t = ( SuccTupIdx ZeroTupIdx `Prj` t
           , ZeroTupIdx `Prj` t)

untup3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> (Exp a, Exp b, Exp c)
untup3 t = ( SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
           , SuccTupIdx ZeroTupIdx `Prj` t
           , ZeroTupIdx `Prj` t)


arr2 :: (Arrays arrs, ArrRepr arrs ~ (((), a), b)) => (Acc a, Acc b) -> Acc arrs
arr2 (xs, ys) = Atuple $ NilTup `SnocTup` xs `SnocTup` ys

arr3 :: (Arrays arrs, ArrRepr arrs ~ ((((), a), b), c)) => (Acc a, Acc b, Acc c) -> Acc arrs
arr3 (xs, ys, zs) = Atuple $ NilTup `SnocTup` xs `SnocTup` ys `SnocTup` zs

unarr2 :: (Arrays arrs, ArrRepr arrs ~ (((), a), b)) => Acc arrs -> (Acc a, Acc b)
unarr2 t = ( SuccTupIdx ZeroTupIdx `Aprj` t
           , ZeroTupIdx `Aprj` t )

unarr3 :: (Arrays arrs, ArrRepr arrs ~ ((((), a), b), c)) => Acc arrs -> (Acc a, Acc b, Acc c)
unarr3 t = ( SuccTupIdx (SuccTupIdx ZeroTupIdx) `Aprj` t
           , SuccTupIdx ZeroTupIdx `Aprj` t
           , ZeroTupIdx `Aprj` t)


-- Conversions
-- -----------

fromIntegral :: (Elt a, Elt b, IsIntegral a, IsFloating b) => Exp a -> Exp b
fromIntegral x = AST.PrimFromInt integralType numType `PrimApp` x


-- Instances
-- ---------

instance Arrays a => Show (Acc a) where
  show = show . convertAcc

instance Show (Exp e) where
  show = show . convertOpenExp EmptyLayout EmptyLayout

instance Elt e => Eq (Exp e) where
  (==) = error "Prelude.(==) applied to EDSL types"

instance (IsNum e, Elt e) => Num (Exp e) where
  x + y       = AST.PrimAdd numType `PrimApp` tup2 (x,y)
  x * y       = AST.PrimMul numType `PrimApp` tup2 (x,y)
  fromInteger = Const . fromInteger

  abs    = error "abs: not defined"
  signum = error "signum: not defined"

