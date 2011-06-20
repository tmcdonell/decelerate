> {-# LANGUAGE GADTs, RankNTypes, StandaloneDeriving #-}

> module STLC where

Here are your typed de Buijn indices (in the style of Altenkirch and
Reus's CSL 1999 paper).

> data Var env ty where                     -- a var is either
>   Top ::               Var (env, ty)   ty -- at the top of the environment, or
>   Pop :: Var env ty -> Var (env, junk) ty -- under some junk

And then your typed lambda terms, with Lam extending an environment.

> data Term env ty where      -- also indexed by environment and type
>   Var  :: Var env ty                            -> Term env ty
>   (:$) :: Term env (dom -> ran) -> Term env dom -> Term env ran
>   Lam  :: Term (env, dom) ran                   -> Term env (dom -> ran)

> deriving instance Show (Var env ty)
> deriving instance Show (Term env ty)

OK, now the crafty bit. We want to do stuff like renaming, substitution,
etc. We have an operation on variables and we want to push it
structurally through terms, applying it at each variable, right?
We'll have some sort of type-preserving but environment-changing operation

 v :: forall ty. Var env ty -> f env' ty

where f might be Var itself (for renamings) or Term (for substitutions)
and we want to jack it up to an operation which traverses terms and
rebuilds them after doing v to the variables.

 rebuild v :: forall ty. Term env ty -> Term env' ty

The following class tells you what you need to know about f if you want
to be able to rebuild terms.

> class Syntactic f where  -- what do we need f to support?
>   varIn    :: Var env ty -> f env ty         -- variables embed in f
>   termOut  :: f env ty   -> Term env ty      -- f embeds in terms
>   weaken   :: f env ty   -> f (env, junk) ty -- f allows weakening

Weakening is something you usually take for granted: every time you
learn a new word, old sentences still make sense. If a conclusion is
justified from some hypotheses, it's still justified if you add more
hypotheses. A term remains in scope if you bind more (fresh) variables.
Weakening is the operation of shifting things from one scope to a larger
scope in which new things have become meaningful, but no old things have
vanished. When you use a named representation (or HOAS representation),
you get weakening for free. But in a de Bruijn representation, weakening
takes work: you have to shift all the variable references to make room
for the new bindings.

Let's do it. First, you need to explain how to shift your v operation
into an extended environment. You'll need to say what to do with the new
variable, and how to account for the new variable on the output side.

> shift ::  Syntactic f =>
>           (forall ty. Var env ty -> f env' ty) ->
>           (forall ty. Var (env, junk) ty -> f (env', junk) ty)
>
> shift _ Top     = varIn Top
> shift v (Pop s) = weaken (v s)

Now develop rebuild, by recursion on the input Term, using shift in the
Lam case:

> rebuild ::  Syntactic f =>
>             (forall ty. Var env ty -> f env' ty) ->
>             (forall ty. Term env ty -> Term env' ty)
>
> rebuild v (Var ix)   = termOut (v ix)
> rebuild v (f :$ x)   = rebuild v f :$ rebuild v x
> rebuild v (Lam body) = Lam $ rebuild (shift v) body

To finish the job, show that Var is Syntactic...

> instance Syntactic Var where
>   varIn   = id
>   termOut = Var
>   weaken  = Pop

...and thence that Term is!

> instance Syntactic Term where
>   varIn   = Var
>   termOut = id
>   weaken  = rebuild weaken


> subTop :: Term env s -> Var (env, s) t -> Term env t
> subTop s Top     = s
> subTop _ (Pop i) = Var i

*STLC> rebuild (subTop (Lam (Var Top))) (Var Top :$ (Var (Pop Top) :$ Lam (Var (Pop Top) :$ Var Top)))
Lam (Var Top) :$ (Var Top :$ Lam (Lam (Var Top) :$ Var Top))



Some extra hints for converting one-variable substitution into simultaneous
substitution, using the GADT Ins as evidence for how to concatenate the prefix
and suffix of an environment after insertion.

> data Ins env t env' where
>   TopIns :: Ins (env, s) s env
>   PopIns :: Ins env t env' -> Ins (env, s) t (env', s)

> thick :: Ins env s env' -> Var env t -> f s -> (Var env' t -> f t) -> f t
> thick TopIns      Top      yes _no  = yes
> thick TopIns      (Pop i) _yes  no  = no i
> thick (PopIns _)  Top     _yes  no  = no Top
> thick (PopIns j)  (Pop i)  yes  no  = thick j i yes (no . Pop)

> thin :: Ins env s env' -> Var env' t -> Var env t
> thin TopIns      i        = Pop i
> thin (PopIns _)  Top      = Top
> thin (PopIns j)  (Pop i)  = Pop (thin j i)

> thinTm :: Ins env s env' -> Term env' t -> Term env t
> thinTm j (Var i) = Var (thin j i)
> thinTm j (f :$ a) = thinTm j f :$ thinTm j a
> thinTm j (Lam t) = Lam (thinTm (PopIns j) t)

> sub1 :: Ins env s env' -> Term env' s -> Term env t -> Term env' t
> sub1 j s (Var i)   = thick j i s Var
> sub1 j s (f :$ a)  = sub1 j s f :$ sub1 j s a
> sub1 j s (Lam t)   = Lam (sub1 (PopIns j) (thinTm TopIns s) t)

