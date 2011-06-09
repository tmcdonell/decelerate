{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import AST
import Type
import Tuple
import Array.Sugar
import Array.Arrays
import qualified Array.Representation as Repr
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
    Unit e      -> unitOp (evalOpenExp e Empty aenv)
    Map f a     -> mapOp (evalFun f aenv) (evalOpenAcc a aenv)
    Fold f x a  -> foldOp (evalFun f aenv) (evalOpenExp x Empty aenv) (evalOpenAcc a aenv)


evalOpenExp :: OpenExp env aenv e -> Val env -> Val aenv -> e
evalOpenExp e env aenv =
  case e of
    Let x b     -> let x' = evalOpenExp x env aenv
                   in  evalOpenExp b (env `Push` x') aenv
    Var ix      -> prj ix env
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
  \x -> evalOpenFun f (env `Push` x) aenv


evalPrim :: PrimFun p -> p
evalPrim (PrimAdd ty) = evalAdd ty
evalPrim (PrimMul ty) = evalMul ty

evalAdd :: NumType a -> ((a,a) -> a)
evalAdd (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (+)
evalAdd (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (+)

evalMul:: NumType a -> ((a,a) -> a)
evalMul (IntegralNumType ty) | IntegralDict <- integralDict ty = uncurry (*)
evalMul (FloatingNumType ty) | FloatingDict <- floatingDict ty = uncurry (*)


-- Array operations
-- ----------------

unitOp :: Elt e
       => e
       -> Scalar e
unitOp e = newArray Z (const e)

mapOp :: forall sh arrs a r. (UniformArrays sh arrs a, Elt r)
      => (a -> r)
      -> arrs
      -> Array sh r
mapOp f arrs =
  newArray (intersectArrays (undefined::a) arrs) (f . indexArrays arrs)

foldOp :: forall sh arrs e. (UniformArrays (sh:.Int) arrs e, Shape sh)
       => (e -> e -> e)
       -> e
       -> arrs
       -> Array sh e
foldOp f e arrs =
  let (sh:.n) = intersectArrays e arrs :: (sh:.Int)
  in  newArray sh (\ix -> iter (Z:.n) (\(Z:.i) -> indexArrays arrs (ix:.i)) f e)


-- Arrays
-- ------

indexArrays :: forall sh arrs e. UniformArrays sh arrs e
            => arrs
            -> sh
            -> e
indexArrays arrs ix
  = toAElt ix arrs
  $ indexR (uniform ix arrs (undefined::e)) (fromArr arrs)
  where
    indexR :: UniformR sh a e' -> a -> AEltRepr a
    indexR UniformRunit  ()              = ()
    indexR UniformRarray arr             = ((), arr ! ix)
    indexR (UniformRpair r1 r2) (a1, a2) = (indexR r1 a1, indexR' r2 a2)
    --
    indexR' :: UniformR sh a e' -> a -> AEltRepr' a
    indexR' UniformRunit  ()              = ()
    indexR' UniformRarray arr             = arr ! ix
    indexR' (UniformRpair r1 r2) (a1, a2) = (indexR r1 a1, indexR' r2 a2)

intersectArrays :: forall arrs sh e. UniformArrays sh arrs e
                => e    {- dummy -}
                -> arrs
                -> sh
intersectArrays e arrs
  = toElt $ intersectR (uniform (undefined::sh) arrs e) (fromArr arrs)
  where
    intersectR :: UniformR sh a e' -> a -> EltRepr sh
    intersectR UniformRunit         ()           = Repr.all
    intersectR UniformRarray        (Array sh _) = sh
    intersectR (UniformRpair r1 r2) (a1, a2)     =
      intersectR r1 a1 `Repr.intersect` intersectR r2 a2


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

