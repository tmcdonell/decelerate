{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module AST where

import Array.Sugar
import Array.Arrays
import Tuple


-- Valuation environments and de Bruijn indices
-- --------------------------------------------

data Idx env t where
  ZeroIdx ::		  Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t

data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val
prj _             _            = error "prj: inconsistent valuation"


-- Array computations
-- ------------------

type Acc a = OpenAcc () a

data OpenAcc aenv a where
  Alet      :: (Arrays a, Arrays b)
            => OpenAcc aenv      a
            -> OpenAcc (aenv, a) b
            -> OpenAcc aenv      b

  Avar      :: Arrays arrs
            => Idx aenv arrs
            -> OpenAcc aenv arrs

  Use       :: Arrays arrs
            => ArrRepr arrs
            -> OpenAcc aenv arrs

  Map       :: (Shape sh, Elt a, Elt r)
            => Fun aenv (a -> r)
            -> OpenAcc aenv (Array sh a)
            -> OpenAcc aenv (Array sh r)

  Fold      :: (Elt e, Shape sh)
            => Fun aenv (e -> e -> e)
            -> Exp aenv e
            -> OpenAcc aenv (Array (sh:.Int) e)
            -> OpenAcc aenv (Array sh e)


-- Embedded expressions
-- --------------------


type Fun aenv t = OpenFun () aenv t

data OpenFun env aenv t where
  Body :: OpenExp env aenv t              -> OpenFun env aenv t
  Lam  :: Elt a
       => OpenFun (env, EltRepr a) aenv t -> OpenFun env aenv (a -> t)


type Exp aenv e = OpenExp () aenv e

data OpenExp env aenv e where
  Let         :: Elt t
              => OpenExp env              aenv t
              -> OpenExp (env, EltRepr t) aenv e
              -> OpenExp env              aenv e

  Var         :: Elt t
              => Idx env (EltRepr t)
              -> OpenExp env aenv t

  Const       :: Elt t
              => EltRepr t
              -> OpenExp env aenv t

  Prj         :: (Elt t, IsTuple t)
              => TupleIdx (TupleRepr t) e
              -> OpenExp env aenv t
              -> OpenExp env aenv e

  Tuple       :: (Elt t, IsTuple t)
              => Tuple (OpenExp env aenv) (TupleRepr t)
              -> OpenExp env aenv t

  PrimApp     :: (Elt a, Elt b)
              => PrimFun (a -> b)
              -> OpenExp env aenv a
              -> OpenExp env aenv b

  IndexScalar :: (Shape sh, Elt e)
              => OpenAcc aenv (Array sh e)
              -> OpenExp env aenv sh
              -> OpenExp env aenv e


data PrimFun sig where
  PrimAdd :: Num a => PrimFun ((a, a) -> a)
  PrimMul :: Num a => PrimFun ((a, a) -> a)

