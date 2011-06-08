{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- Substitution of open array and scalar expressions into a variety of contexts.
-- Adapted from work originally by Sean Seefried.
--

module Substitution (
  Cat,
  substituteOpenAcc, substituteOpenExp
) where

import AST
import Tuple
import Prelude hiding (exp)


-- Type environment
-- ----------------

-- Represents where in an environment we wish to insert a new de Bruijn index.
-- If the original environment is equal to `Cat env benv` then the new index is
-- inserted between `env` and `benv` (the corresponding bound environment).
--
data Insert env benv t where
  Base  ::                      Insert env () t
  Shift :: Insert env benv t -> Insert env (benv, s) t

-- Concatenation of two type environments, represented as nested tuples
--
type family Cat env env'
type instance Cat env ()        = env
type instance Cat env (env', s) = (Cat env env', s)

-- Type equality
--
data s :=: t where
  Refl :: s :=: s


-- Testing and extending indices
-- -----------------------------

equiv :: Insert env benv s -> Idx (Cat (env, s) benv) t -> Maybe (s :=: t)
equiv Base      ZeroIdx       = Just Refl
equiv (Shift n) (SuccIdx idx) = equiv n idx
equiv _         _             = Nothing

extend :: forall env benv s t. env -> Idx (benv, s) t -> Idx (Cat env benv, s) t
extend _ ZeroIdx               = ZeroIdx
extend _ (SuccIdx ZeroIdx)     = SuccIdx ZeroIdx
extend _ (SuccIdx (SuccIdx n)) = SuccIdx (extend (undefined::env) (SuccIdx n))

lift :: Insert env benv s -> Idx (Cat env benv) t -> Idx (Cat (env, s) benv) t
lift Base      n           = SuccIdx n
lift (Shift _) ZeroIdx     = ZeroIdx
lift (Shift n) (SuccIdx m) = SuccIdx (lift n m)

sink :: Insert env benv s -> Idx (Cat (env, s) benv) t -> Idx (Cat env benv) t
sink Base      ZeroIdx     = error "internal error: inconsistent valuation"
sink Base      (SuccIdx m) = m
sink (Shift _) ZeroIdx     = ZeroIdx
sink (Shift n) (SuccIdx m) = SuccIdx (sink n m)


-- Lifting
-- -------
--
-- A family of lifting functions that increment all free variables in either a
-- scalar or array environment. Functions are suffixed to indicate which
-- environment they manipulate.
--

-- Array environments
--
liftOpenAccA :: Insert aenv baenv s
             -> OpenAcc (Cat aenv      baenv) a
             -> OpenAcc (Cat (aenv, s) baenv) a
liftOpenAccA n acc =
  case acc of
    Alet a b    -> Alet (liftOpenAccA n a) (liftOpenAccA (Shift n) b)
    Avar ix     -> Avar (lift n ix)
    Aprj ix a   -> Aprj ix (liftOpenAccA n a)
    Atuple tup  -> Atuple (liftATupleA n tup)
    Use a       -> Use a
    Map f a     -> Map (liftOpenFunA n f) (liftOpenAccA n a)
    Fold f e a  -> Fold (liftOpenFunA n f) (liftOpenExpA n e) (liftOpenAccA n a)


liftOpenExpA :: Insert aenv baenv s
             -> OpenExp env (Cat aenv     baenv) e
             -> OpenExp env (Cat (aenv,s) baenv) e
liftOpenExpA n exp =
  case exp of
    Let a b          -> Let (liftOpenExpA n a) (liftOpenExpA n b)
    Var ix           -> Var ix
    Const c          -> Const c
    Prj ix e         -> Prj ix (liftOpenExpA n e)
    Tuple tup        -> Tuple (liftTupleA n tup)
    PrimApp f e      -> PrimApp f (liftOpenExpA n e)
    IndexScalar a ix -> IndexScalar (liftOpenAccA n a) (liftOpenExpA n ix)


liftOpenFunA :: Insert aenv baenv s
             -> OpenFun env (Cat aenv      baenv) t
             -> OpenFun env (Cat (aenv, s) baenv) t
liftOpenFunA n fun =
  case fun of
    Body e -> Body (liftOpenExpA n e)
    Lam  f -> Lam  (liftOpenFunA n f)


liftTupleA :: Insert aenv baenv s
           -> Tuple (OpenExp env (Cat aenv     baenv)) t
           -> Tuple (OpenExp env (Cat (aenv,s) baenv)) t
liftTupleA n tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> liftTupleA n t `SnocTup` liftOpenExpA n e


liftATupleA :: Insert aenv baenv s
            -> Tuple (OpenAcc (Cat aenv      baenv)) a
            -> Tuple (OpenAcc (Cat (aenv, s) baenv)) a
liftATupleA n tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t a -> liftATupleA n t `SnocTup` liftOpenAccA n a


-- Scalar environments
--
liftOpenExpE :: Insert env benv s
             -> OpenExp (Cat env     benv) aenv t
             -> OpenExp (Cat (env,s) benv) aenv t
liftOpenExpE n exp =
  case exp of
    Let a b          -> Let (liftOpenExpE n a) (liftOpenExpE (Shift n) b)
    Var ix           -> Var (lift n ix)
    Const c          -> Const c
    Prj ix e         -> Prj ix (liftOpenExpE n e)
    Tuple tup        -> Tuple (liftTupleE n tup)
    PrimApp f e      -> PrimApp f (liftOpenExpE n e)
    IndexScalar a ix -> IndexScalar a (liftOpenExpE n ix)


liftTupleE :: Insert env benv s
           -> Tuple (OpenExp (Cat env     benv) aenv) t
           -> Tuple (OpenExp (Cat (env,s) benv) aenv) t
liftTupleE n tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> liftTupleE n t `SnocTup` liftOpenExpE n e


-- Substitution
-- ------------
--
-- A family of functions to substitute an open scalar or array expression into
-- an entity, in place of a provided de Bruijn index.
--
-- Functions are named by the type of expression we are substituting into, and
-- suffixed to indicate what is being substituted.
--

-- Array environments
--
subOpenAccA :: Insert aenv baenv s
            -> OpenAcc (Cat aenv      baenv) s  -- substitute this...
            -> OpenAcc (Cat (aenv, s) baenv) t  -- ...into this
            -> OpenAcc (Cat aenv      baenv) t
subOpenAccA n acc into =
  case into of
    Alet a b    -> Alet (subOpenAccA n acc a) (subOpenAccA (Shift n) (liftOpenAccA Base acc) b)
    Avar ix
      | Just Refl <- equiv n ix -> acc
      | otherwise               -> Avar (sink n ix)

    Aprj ix a   -> Aprj ix (subOpenAccA n acc a)
    Use a       -> Use a
    Atuple tup  -> Atuple (subAtupleA n acc tup)
    Map f a     -> Map (subOpenFunA n acc f) (subOpenAccA n acc a)
    Fold f e a  -> Fold (subOpenFunA n acc f) (subOpenExpA n acc e) (subOpenAccA n acc a)


subOpenExpA :: Insert aenv baenv s
            -> OpenAcc     (Cat aenv      baenv) s
            -> OpenExp env (Cat (aenv, s) baenv) t
            -> OpenExp env (Cat aenv      baenv) t
subOpenExpA n acc exp =
  case exp of
    Let a b          -> Let (subOpenExpA n acc a) (subOpenExpA n acc b)
    Var ix           -> Var ix
    Const c          -> Const c
    Prj ix e         -> Prj ix (subOpenExpA n acc e)
    Tuple tup        -> Tuple (subTupleA n acc tup)
    PrimApp f e      -> PrimApp f (subOpenExpA n acc e)
    IndexScalar a ix -> IndexScalar (subOpenAccA n acc a) (subOpenExpA n acc ix)


subOpenFunA :: Insert aenv baenv s
            -> OpenAcc     (Cat aenv      baenv) s
            -> OpenFun env (Cat (aenv, s) baenv) t
            -> OpenFun env (Cat aenv      baenv) t
subOpenFunA n acc fun =
  case fun of
    Body e -> Body (subOpenExpA n acc e)
    Lam  f -> Lam  (subOpenFunA n acc f)


subTupleA :: Insert aenv baenv s
          -> OpenAcc            (Cat aenv      baenv)  s
          -> Tuple (OpenExp env (Cat (aenv, s) baenv)) t
          -> Tuple (OpenExp env (Cat aenv      baenv)) t
subTupleA n acc tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> subTupleA n acc t `SnocTup` subOpenExpA n acc e


subAtupleA :: Insert aenv baenv s
           -> OpenAcc        (Cat aenv      baenv)  s
           -> Tuple (OpenAcc (Cat (aenv, s) baenv)) t
           -> Tuple (OpenAcc (Cat aenv      baenv)) t
subAtupleA n acc tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t a -> subAtupleA n acc t `SnocTup` subOpenAccA n acc a


-- Scalar environments
--
subOpenExpE :: Insert env benv s
            -> OpenExp (Cat env      benv) aenv s       -- substitute this...
            -> OpenExp (Cat (env, s) benv) aenv t       -- ...into this
            -> OpenExp (Cat env      benv) aenv t
subOpenExpE n exp into =
  case into of
    Let a b          -> Let (subOpenExpE n exp a) (subOpenExpE (Shift n) (liftOpenExpE Base exp) b)
    Var ix
      | Just Refl <- equiv n ix -> exp
      | otherwise               -> Var (sink n ix)

    Const c          -> Const c
    Prj ix e         -> Prj ix (subOpenExpE n exp e)
    Tuple tup        -> Tuple (subTupleE n exp tup)
    PrimApp f e      -> PrimApp f (subOpenExpE n exp e)
    IndexScalar a ix -> IndexScalar a (subOpenExpE n exp ix)


subTupleE :: Insert env benv s
          -> OpenExp        (Cat env      benv) aenv  s
          -> Tuple (OpenExp (Cat (env, s) benv) aenv) t
          -> Tuple (OpenExp (Cat env      benv) aenv) t
subTupleE n exp tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> subTupleE n exp t `SnocTup` subOpenExpE n exp e


-- Extending environments
-- ----------------------
--
-- A family of functions to extend a scalar or array environment. Functions are
-- suffixed to indicate which environment they manipulate.
--

-- Array environments
--
extendOpenAccA :: aenv {- dummy -}
               -> OpenAcc (aenv', s)          t
               -> OpenAcc (Cat aenv aenv', s) t
extendOpenAccA prf acc =
  case acc of
    Alet a b    -> Alet (extendOpenAccA prf a) (extendOpenAccA prf b)
    Avar ix     -> Avar (extend prf ix)
    Aprj ix a   -> Aprj ix (extendOpenAccA prf a)
    Atuple tup  -> Atuple (extendAtupleA prf tup)
    Use a       -> Use a
    Map f a     -> Map (extendOpenFunA prf f) (extendOpenAccA prf a)
    Fold f e a  -> Fold (extendOpenFunA prf f) (extendOpenExpA prf e) (extendOpenAccA prf a)


extendOpenExpA :: aenv {- dummy -}
               -> OpenExp env (aenv', s)          t
               -> OpenExp env (Cat aenv aenv', s) t
extendOpenExpA prf exp =
  case exp of
    Let a b          -> Let (extendOpenExpA prf a) (extendOpenExpA prf b)
    Var ix           -> Var ix
    Const c          -> Const c
    Prj ix e         -> Prj ix (extendOpenExpA prf e)
    Tuple tup        -> Tuple (extendTupleA prf tup)
    PrimApp f e      -> PrimApp f (extendOpenExpA prf e)
    IndexScalar a ix -> IndexScalar (extendOpenAccA prf a) (extendOpenExpA prf ix)


extendOpenFunA :: aenv {- dummy -}
               -> OpenFun env (aenv', s)          t
               -> OpenFun env (Cat aenv aenv', s) t
extendOpenFunA prf fun =
  case fun of
    Body e -> Body (extendOpenExpA prf e)
    Lam  f -> Lam  (extendOpenFunA prf f)


extendTupleA :: aenv {- dummy -}
             -> Tuple (OpenExp env (aenv', s))          t
             -> Tuple (OpenExp env (Cat aenv aenv', s)) t
extendTupleA prf tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> extendTupleA prf t `SnocTup` extendOpenExpA prf e


extendAtupleA :: aenv {- dummy -}
              -> Tuple (OpenAcc (aenv', s))          t
              -> Tuple (OpenAcc (Cat aenv aenv', s)) t
extendAtupleA prf tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t a -> extendAtupleA prf t `SnocTup` extendOpenAccA prf a


-- Scalar environments
--
extendOpenExpE :: env {- dummy -}
               -> OpenExp (env', s)         aenv t
               -> OpenExp (Cat env env', s) aenv t
extendOpenExpE prf exp =
  case exp of
    Let a b          -> Let (extendOpenExpE prf a) (extendOpenExpE prf b)
    Var ix           -> Var (extend prf ix)
    Const c          -> Const c
    Prj ix e         -> Prj ix (extendOpenExpE prf e)
    Tuple tup        -> Tuple (extendTupleE prf tup)
    PrimApp f e      -> PrimApp f (extendOpenExpE prf e)
    IndexScalar a ix -> IndexScalar a (extendOpenExpE prf ix)


extendTupleE :: env {- dummy -}
             -> Tuple (OpenExp (env', s)         aenv) t
             -> Tuple (OpenExp (Cat env env', s) aenv) t
extendTupleE prf tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> extendTupleE prf t `SnocTup` extendOpenExpE prf e


-- Substitution
-- ------------
--
-- A family of function to substitute a term into an entity at the head of the
-- environment (de Bruijn index zero).
--
substituteOpenAcc :: forall aenv aenv' s t.
                     OpenAcc (aenv', s)       t
                  -> OpenAcc (Cat aenv aenv') s
                  -> OpenAcc (Cat aenv aenv') t
substituteOpenAcc into acc =
  subOpenAccA Base acc (extendOpenAccA (undefined::aenv) into)

substituteOpenExp :: forall env env' aenv s t.
                     OpenExp (env', s)      aenv t
                  -> OpenExp (Cat env env') aenv s
                  -> OpenExp (Cat env env') aenv t
substituteOpenExp into exp =
  subOpenExpE Base exp (extendOpenExpE (undefined::env) into)

