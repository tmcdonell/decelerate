{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import AST
import Tuple
import Array.Sugar
import Array.Arrays
import qualified Smart


-- Interpreter
-- -----------

run :: Arrays arrs => Smart.Acc arrs -> arrs
run acc = evalOpenAcc (Smart.convertAcc acc) Empty


evalOpenAcc :: OpenAcc aenv a -> Val aenv -> a
evalOpenAcc acc aenv =
  case acc of
    Alet a b    -> let a' = evalOpenAcc a aenv
                   in  evalOpenAcc b (aenv `Push` a')
    Avar ix     -> prj ix aenv
    Aprj ix a   -> evalPrj ix (fromArr $ evalOpenAcc a aenv)
    Atuple tup  -> evalAtup tup aenv
    Use arr     -> toArr arr
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
    Prj ix t    -> evalPrj ix (fromTuple $ evalOpenExp t env aenv)
    PrimApp f x -> evalPrim f (evalOpenExp x env aenv)
    IndexScalar arr sh
                -> (evalOpenAcc arr aenv) ! evalOpenExp sh env aenv


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
      -> Array dim a
      -> Array dim b
mapOp f arr@(Array sh _) =
  newArray (toElt sh) (\ix -> f (arr ! ix))

foldOp :: (Elt e, Shape dim)
       => (e -> e -> e)
       -> e
       -> Array (dim:.Int) e
       -> Array dim e
foldOp f e arr@(Array (sh,n) _) =
  newArray (toElt sh) (\ix -> iter (Z:.n) (\(Z:.i) -> arr ! (ix:.i)) f e)


-- Tuples
-- ------

evalPrj :: TupleIdx t e -> t -> e
evalPrj ZeroTupIdx       (_,   e) = e
evalPrj (SuccTupIdx ix') (tup, _) = evalPrj ix' tup


evalAtup :: forall aenv arrs. Arrays arrs
         => Tuple (OpenAcc aenv) (ArrRepr arrs)
         -> Val aenv
         -> arrs
evalAtup t aenv = toArr (eval t)
  where
    eval :: Tuple (OpenAcc aenv) tup -> tup
    eval NilTup           = ()
    eval (SnocTup arrs a) = (eval arrs, evalOpenAcc a aenv)

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

