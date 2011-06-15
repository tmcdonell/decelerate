{-# LANGUAGE GADTs #-}

module Substitution.Evidence where

import AST
import Tuple
import Prelude hiding (exp)


-- Evidence of insertion
-- ---------------------

data Insert env env' t where
  ZeroIns ::                      Insert (env, s) env       s
  SuccIns :: Insert env env' t -> Insert (env, s) (env', s) t

-- Every time you learn a new word, old sentences still make sense. If a
-- conclusion is justified by a hypothesis, it is still justified if you add
-- more hypotheses. Similarly, a term remains in scope if we bind more (fresh)
-- variables. Thinning (weakening) is the operation of shifting things from
-- one scope to a larger scope.
--
thin :: Insert env env' s -> Idx env' t -> Idx env t
thin ZeroIns     ix           = SuccIdx ix
thin (SuccIns _) ZeroIdx      = ZeroIdx
thin (SuccIns n) (SuccIdx ix) = SuccIdx (thin n ix)

-- Thick inverts thin. Single substitution obliges us to test equality on
-- variables up front. This neat trick (thanks to Conor McBride) combines both
-- cases; getting equality when variables matches, or extracting the thinned
-- variable otherwise.
--
-- Moreover, this turns a single substitution into simultaneous substitution.
--
thick :: Insert env env' s -> Idx env t -> f s -> (Idx env' t -> f t) -> f t
thick ZeroIns     ZeroIdx      yes _  = yes
thick ZeroIns     (SuccIdx ix) _   no = no ix
thick (SuccIns _) ZeroIdx      _   no = no ZeroIdx
thick (SuccIns n) (SuccIdx ix) yes no = thick n ix yes (no . SuccIdx)



-- Thinning
-- --------

-- Array expressions
--
thinA :: Insert aenv aenv' s -> OpenAcc aenv' a -> OpenAcc aenv a
thinA n acc =
  case acc of
    Alet a b    -> Alet (thinA n a) (thinA (SuccIns n) b)
    Avar ix     -> Avar (thin n ix)
    Aprj ix a   -> Aprj ix (thinA n a)
    Atuple tup  -> Atuple (thinATA n tup)
    Use a       -> Use a
    Unit e      -> Unit (thinEA n e)
    Map f a     -> Map (thinFA n f) (thinA n a)
    Fold f e a  -> Fold (thinFA n f) (thinEA n e) (thinA n a)

thinEA :: Insert aenv aenv' s -> OpenExp env aenv' e -> OpenExp env aenv e
thinEA n exp =
  case exp of
    Let a b          -> Let (thinEA n a) (thinEA n b)
    Var ix           -> Var ix
    Const c          -> Const c
    Prj ix e         -> Prj ix (thinEA n e)
    Tuple tup        -> Tuple (thinTA n tup)
    PrimApp f e      -> PrimApp f (thinEA n e)
    IndexScalar a ix -> IndexScalar (thinA n a) (thinEA n ix)

thinFA :: Insert aenv aenv' s -> OpenFun env aenv' t -> OpenFun env aenv t
thinFA n fun =
  case fun of
    Body e -> Body (thinEA n e)
    Lam  f -> Lam  (thinFA n f)

thinTA :: Insert aenv aenv' s -> Tuple (OpenExp env aenv') t -> Tuple (OpenExp env aenv) t
thinTA n tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> thinTA n t `SnocTup` thinEA n e

thinATA :: Insert aenv aenv' s -> Tuple (OpenAcc aenv') a -> Tuple (OpenAcc aenv) a
thinATA n tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t a -> thinATA n t `SnocTup` thinA n a


-- Scalar expressions
--
thinE :: Insert env env' s -> OpenExp env' aenv t -> OpenExp env aenv t
thinE n exp =
  case exp of
    Let a b          -> Let (thinE n a) (thinE (SuccIns n) b)
    Var ix           -> Var (thin n ix)
    Const c          -> Const c
    Prj ix e         -> Prj ix (thinE n e)
    Tuple tup        -> Tuple (thinTE n tup)
    PrimApp f e      -> PrimApp f (thinE n e)
    IndexScalar a ix -> IndexScalar a (thinE n ix)

thinTE :: Insert env env' s -> Tuple (OpenExp env' aenv) t -> Tuple (OpenExp env aenv) t
thinTE n tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> thinTE n t `SnocTup` thinE n e


-- Substitution
-- ------------

-- Array expressions
--
subA :: Insert aenv aenv' s
     -> OpenAcc aenv' s         -- substitute this...
     -> OpenAcc aenv  t         -- ...into this
     -> OpenAcc aenv' t
subA n acc into =
  case into of
    Avar ix     -> thick n ix acc Avar
    Alet a b    -> Alet (subA n acc a) (subA (SuccIns n) (thinA ZeroIns acc) b)
    Aprj ix a   -> Aprj ix (subA n acc a)
    Use a       -> Use a
    Unit e      -> Unit (subEA n acc e)
    Atuple tup  -> Atuple (subATA n acc tup)
    Map f a     -> Map (subFA n acc f) (subA n acc a)
    Fold f e a  -> Fold (subFA n acc f) (subEA n acc e) (subA n acc a)

subEA :: Insert aenv aenv' s
      -> OpenAcc     aenv' s
      -> OpenExp env aenv  t
      -> OpenExp env aenv' t
subEA n acc exp =
  case exp of
    Let a b          -> Let (subEA n acc a) (subEA n acc b)
    Var ix           -> Var ix
    Const c          -> Const c
    Prj ix e         -> Prj ix (subEA n acc e)
    Tuple tup        -> Tuple (subTA n acc tup)
    PrimApp f e      -> PrimApp f (subEA n acc e)
    IndexScalar a ix -> IndexScalar (subA n acc a) (subEA n acc ix)

subFA :: Insert aenv aenv' s
      -> OpenAcc     aenv' s
      -> OpenFun env aenv  t
      -> OpenFun env aenv' t
subFA n acc fun =
  case fun of
    Body e -> Body (subEA n acc e)
    Lam  f -> Lam  (subFA n acc f)

subTA :: Insert aenv aenv' s
      -> OpenAcc            aenv'  s
      -> Tuple (OpenExp env aenv)  t
      -> Tuple (OpenExp env aenv') t
subTA n acc tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> subTA n acc t `SnocTup` subEA n acc e

subATA :: Insert aenv aenv' s
       -> OpenAcc        aenv'  s
       -> Tuple (OpenAcc aenv)  t
       -> Tuple (OpenAcc aenv') t
subATA n acc tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t a -> subATA n acc t `SnocTup` subA n acc a


-- Scalar expressions
--
subE :: Insert env env' s
     -> OpenExp env' aenv s     -- substitute this...
     -> OpenExp env  aenv t     -- ...into this
     -> OpenExp env' aenv t
subE n exp into =
  case into of
    Var ix           -> thick n ix exp Var
    Let a b          -> Let (subE n exp a) (subE (SuccIns n) (thinE ZeroIns exp) b)
    Const c          -> Const c
    Prj ix e         -> Prj ix (subE n exp e)
    Tuple tup        -> Tuple (subTE n exp tup)
    PrimApp f e      -> PrimApp f (subE n exp e)
    IndexScalar a ix -> IndexScalar a (subE n exp ix)

subTE :: Insert env env' s
      -> OpenExp        env' aenv  s
      -> Tuple (OpenExp env  aenv) t
      -> Tuple (OpenExp env' aenv) t
subTE n exp tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> subTE n exp t `SnocTup` subE n exp e

subFE :: Insert env env' s
      -> OpenExp env' aenv s
      -> OpenFun env  aenv t
      -> OpenFun env' aenv t
subFE n exp fun =
  case fun of
    Body e -> Body (subE n exp e)
    Lam  f -> Lam  (subFE (SuccIns n) (thinE ZeroIns exp) f)

