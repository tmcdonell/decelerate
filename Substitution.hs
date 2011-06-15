{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Substitution (substitute, compose) where

import AST
import Tuple
import Prelude hiding (exp)


-- Wrappers around indices and array expressions such that our Syntactic
-- elements have the same kind. The second environment is propagated unchanged.
--
newtype Idx'     env env' t = I { unI :: Idx env t }
--newtype OpenAcc' env env' a = A { unA :: OpenAcc env a }

-- SEE: [Renaming and Substitution]
-- SEE: [Weakening]
--
class Syntactic f where
  varIn  :: Idx env t    -> f env env' t
  expOut :: f env env' t -> OpenExp env env' t
  weaken :: f env env' t -> f (env, s) env' t

instance Syntactic Idx' where
  varIn  = I
  expOut = Var . unI
  weaken = I . SuccIdx . unI

instance Syntactic OpenExp where
  varIn  = Var
  expOut = id
  weaken = rebuild (weaken . I)


shift :: Syntactic f
      => (forall t'. Idx env t' -> f env' aenv t')
      -> Idx' (env,  s) aenv t
      -> f    (env', s) aenv t
shift _ (I ZeroIdx)      = varIn ZeroIdx
shift v (I (SuccIdx ix)) = weaken (v ix)


rebuild :: Syntactic f
        => (forall t'. Idx env t' -> f env' aenv t')
        -> OpenExp env  aenv t
        -> OpenExp env' aenv t
rebuild v exp =
  case exp of
    Let a b          -> Let (rebuild v a) (rebuild (shift v . I) b)
    Var ix           -> expOut (v ix)
    Const c          -> Const c
    Prj tup e        -> Prj tup (rebuild v e)
    Tuple tup        -> Tuple (rebuildT v tup)
    PrimApp f e      -> PrimApp f (rebuild v e)
    IndexScalar a ix -> IndexScalar a (rebuild v ix)


rebuildT :: Syntactic f
         => (forall t'. Idx env t' -> f env' aenv t')
         -> Tuple (OpenExp env  aenv) t
         -> Tuple (OpenExp env' aenv) t
rebuildT v tup =
  case tup of
    NilTup      -> NilTup
    SnocTup t e -> rebuildT v t `SnocTup` rebuild v e


-- Replace the first variable (ZeroIdx) with the given expression.
--
substitute :: OpenExp (env, s) aenv t
           -> OpenExp env      aenv s
           -> OpenExp env      aenv t
substitute f g = rebuild (subTop g) f
  where
    subTop :: OpenExp env aenv s -> Idx (env, s) t -> OpenExp env aenv t
    subTop s ZeroIdx      = s
    subTop _ (SuccIdx ix) = Var ix


-- Composition of unary functions
--
compose :: OpenFun env aenv (b -> c)
        -> OpenFun env aenv (a -> b)
        -> OpenFun env aenv (a -> c)
compose (Lam (Body f)) (Lam (Body g)) = Lam . Body $ substitute (rebuild extend f) g
compose _              _              = error "impossible evaluation"


-- We don't have shift or weaken for OpenFun, which we would otherwise apply in
-- a recursive case. Use this instead, which will work for our unary functions.
--
extend :: Idx (env, s) t -> OpenExp ((env, s'), s) aenv t
extend = shift (varIn . SuccIdx) . varIn


{--
foo :: OpenFun env aenv t
    -> OpenExp env aenv s
    -> OpenFun env aenv t
foo fun exp =
  case fun of
    Lam  f -> Lam  $ foo f (weaken exp)
    Body e -> Body $ substitute (weaken e) exp
--}


-- NOTE: [Renaming and Substitution]
--
-- To do things like renaming and substitution, we need some operation on
-- variables that we push structurally through terms, applying to each variable.
-- We have a type preserving but environment changing operation:
--
--   v :: forall t. Idx env t -> f env' aenv t
--
-- The crafty bit is that 'f' might represent variables (for renaming) or terms
-- (for substitutions). The demonic forall, --- which is to say that the
-- quantifier is in a position which gives us obligation, not opportunity ---
-- forces us to respect type: when pattern matching detects the variable we care
-- about, happily we discover that it has the type we must respect. The demon is
-- not so free to mess with us as one might fear at first.
--
-- We then lift this to an operation which traverses terms and rebuild them
-- after applying 'v' to the variables:
--
--   rebuild v :: OpenExp env aenv t -> OpenExp env' aenv t
--
-- The Syntactic class tells us what we need to know about 'f' if we want to be
-- able to rebuild terms. In essence, the crucial functionality is to propagate
-- a class of operations on variables that is closed under shifting.
--

-- NOTE: [Weakening]
--
-- Weakening is something we usually take for granted: every time you learn a
-- new word, old sentences still make sense. If a conclusion is justified by a
-- hypothesis, it is still justified if you add more hypotheses. Similarly, a
-- term remains in scope if you bind more (fresh) variables. Weakening is the
-- operation of shifting things from one scope to a larger scope in which new
-- things have become meaningful, but no old things have vanished.
--
-- When we use a named representation (or HOAS) we get weakening for free, we
-- get this for free. But in the de Bruijn representation weakening takes work:
-- you have to shift all variables references to make room for the new bindings.
--
