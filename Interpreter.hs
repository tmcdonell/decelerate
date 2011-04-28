{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import AST
import Tuple
import Array.Sugar
import Array.Arrays
import Array.Delayed
import qualified Smart


-- Interpreter
-- -----------

run :: Arrays arrs => Smart.Acc arrs -> arrs
run acc = force $ evalOpenAcc (Smart.convertAcc acc) Empty


evalOpenAcc :: Delayable a => OpenAcc aenv a -> Val aenv -> Delayed a
evalOpenAcc acc aenv =
  case acc of
    Alet a b    -> let a' = force $ evalOpenAcc a aenv
                   in  evalOpenAcc b (aenv `Push` a')
    Avar ix     -> delay $ prj ix aenv
    Aprj ix a   -> delay . evalAprj ix . force $ evalOpenAcc a aenv
    Use arr     -> delay $ toArr arr
    Map f a     -> mapOp (evalFun f aenv) (evalOpenAcc a aenv)
    Fold f x a  -> foldOp (evalFun f aenv) (evalOpenExp x Empty aenv) (evalOpenAcc a aenv)


evalOpenExp :: OpenExp env aenv e -> Val env -> Val aenv -> e
evalOpenExp e env aenv =
  case e of
    Let x b     -> let x' = evalOpenExp x env aenv
                   in  evalOpenExp b (env `Push` fromElt x') aenv
    Var ix      -> toElt $ prj ix env
    Const t     -> toElt t
    Tuple t     -> evalTup t env aenv
    Prj ix t    -> evalPrj ix (evalOpenExp t env aenv)
    PrimApp f x -> evalPrim f (evalOpenExp x env aenv)
    IndexScalar arr sh
                -> (force $ evalOpenAcc arr aenv) ! evalOpenExp sh env aenv


evalFun :: Fun aenv t -> Val aenv -> t
evalFun f aenv = evalOpenFun f Empty aenv

evalOpenFun :: OpenFun env aenv t -> Val env -> Val aenv -> t
evalOpenFun (Body e) env aenv = evalOpenExp e env aenv
evalOpenFun (Lam  f) env aenv =
  \x -> evalOpenFun f (env `Push` fromElt x) aenv


evalPrim :: PrimFun p -> p
evalPrim PrimAdd = uncurry (+)
evalPrim PrimMul = uncurry (*)


-- Array operations
-- ----------------

mapOp :: (Elt a, Elt b)
      => (a -> b)
      -> Delayed (Array dim a)
      -> Delayed (Array dim b)
mapOp f (DelayedArray sh rf) =
  DelayedArray sh (sinkFromElt f . rf)

foldOp :: (Elt e, Shape dim)
       => (e -> e -> e)
       -> e
       -> Delayed (Array (dim:.Int) e)
       -> Delayed (Array dim e)
foldOp f e (DelayedArray (sh,n) rf) =
  DelayedArray sh (\ix -> iter (Z:.n) (\(Z:.i) -> rf (ix,i)) (sinkFromElt2 f) (fromElt e))


-- Tuples
-- ------

evalAprj :: Arrays arrs => TupleIdx (ArrRepr arrs) a -> arrs -> a
evalAprj ix = eval ix . fromArr
  where
    eval :: TupleIdx arrs a -> arrs -> a
    eval ZeroTupIdx       (_,   a) = a
    eval (SuccTupIdx ix') (tup, _) = eval ix' tup

evalPrj :: IsTuple t => TupleIdx (TupleRepr t) e -> t -> e
evalPrj ix = eval ix . fromTuple
  where
    eval :: TupleIdx t e -> t -> e
    eval ZeroTupIdx       (_,   e) = e
    eval (SuccTupIdx ix') (tup, _) = eval ix' tup


evalTup :: forall env aenv t. IsTuple t
        => Tuple (OpenExp env aenv) (TupleRepr t)
        -> Val env
        -> Val aenv
        -> t
evalTup t env aenv = toTuple (eval t)
  where
    eval :: Tuple (OpenExp env aenv) tup -> tup
    eval NilTup         = ()
    eval (SnocTup es e) = (eval es, evalOpenExp e env aenv)

