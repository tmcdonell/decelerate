{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pretty where

import AST
import Tuple
import Array.Sugar
import Array.Arrays
import Text.PrettyPrint


instance Show (OpenAcc aenv a) where
  show = render . prettyOpenAcc 0 id

instance Show (OpenExp env aenv e) where
  show = render . prettyOpenExp 0 0 id

instance Show (OpenFun env aenv f) where
  show = render . prettyOpenFun 0 0



-- Array computations
-- ------------------

prettyOpenAcc :: forall aenv a. Int -> (Doc -> Doc) -> OpenAcc aenv a -> Doc
prettyOpenAcc !alvl wrap acc =
  case acc of
    Avar ix  -> char 'a' <> int (alvl - idxToInt ix - 1)
    Alet x y ->
      let var  = char 'a' <> int alvl
          lam  = prettyOpenAcc alvl id x
          body = prettyOpenAcc (alvl+1) id y
      in
      sep [ hang (text "let" <+> var <+> char '=') 2 lam
          ,       text "in"  <+> body ]

    Aprj ix arrs   -> wrap $ int (tupleIdxToInt ix) <+> prettyOpenAcc alvl parens arrs
    Atuple tup     -> prettyAtuple alvl tup
    Use arrs       -> wrap $ prettyAccOp "use" [ prettyArrays (arrays (undefined::a)) arrs ]

    Map f arr      -> wrap $ prettyAccOp "map" [ prettyOpenFun 0 alvl f
                                               , prettyOpenAcc alvl parens arr]

    Fold f e arr   -> wrap $ prettyAccOp "fold" [ prettyOpenFun 0 alvl f
                                                , prettyOpenExp 0 alvl parens e
                                                , prettyOpenAcc alvl parens arr]


-- Scalar expressions
-- ------------------

prettyOpenFun :: Int -> Int -> OpenFun env aenv t -> Doc
prettyOpenFun !lvl !alvl fun = parens $
  char '\\' <> hsep [char 'x' <> int (lvl+i) | i <- [0..n]] <+> text "->" <+> body
  where
    (n, body) = trav n fun

    trav :: Int -> OpenFun env aenv t -> (Int, Doc)
    trav l (Body e) = (-1, prettyOpenExp l alvl id e)
    trav l (Lam  f) = let (n',b) = trav l f in (n'+1, b)


prettyOpenExp :: forall env aenv t. Int -> Int -> (Doc -> Doc) -> OpenExp env aenv t -> Doc
prettyOpenExp !lvl !alvl wrap e =
  case e of
    Var ix  -> char 'x' <> int (lvl - idxToInt ix)
    Let x y ->
      let var  = char 'x' <> int lvl
          lam  = prettyOpenExp lvl alvl id x
          body = prettyOpenExp (lvl+1) alvl id y
      in
      sep [ hang (text "let" <+> var <+> char '=') 2 lam
          ,       text "in"  <+> body ]

    Const v            -> text $ show (toElt v :: t)
    Prj ix t           -> wrap $ int (tupleIdxToInt ix) <+> prettyOpenExp lvl alvl parens t
    Tuple t            -> prettyTuple lvl alvl t
    PrimApp f x        -> wrap $ prettyPrim f <+> prettyOpenExp lvl alvl parens x
    IndexScalar arr sh -> wrap $ cat [ prettyOpenAcc alvl parens arr
                                     , char '!'
                                     , prettyOpenExp lvl alvl parens sh ]

prettyPrim :: PrimFun a -> Doc
prettyPrim (PrimAdd _) = text "(+)"
prettyPrim (PrimMul _) = text "(*)"


-- Tuples
-- ------

prettyAtuple :: Int -> Tuple (OpenAcc aenv) t -> Doc
prettyAtuple !alvl t = parens . sep . punctuate comma . reverse $ collect t
  where
    collect :: Tuple (OpenAcc aenv) t -> [Doc]
    collect NilTup          = []
    collect (SnocTup tup a) = prettyOpenAcc alvl id a : collect tup

prettyTuple :: Int -> Int -> Tuple (OpenExp env aenv) t -> Doc
prettyTuple !lvl !alvl t = parens . sep . punctuate comma . reverse $ collect t
  where
    collect :: Tuple (OpenExp env aenv) t -> [Doc]
    collect NilTup          = []
    collect (SnocTup tup e) = prettyOpenExp lvl alvl id e : collect tup


-- Auxiliary
-- ---------

prettyAccOp :: String -> [Doc] -> Doc
prettyAccOp name docs = hang (text name) 2 (sep docs)


prettyArrays :: ArraysR arrs -> arrs -> Doc
prettyArrays arrs = parens . sep . punctuate comma . collect arrs
  where
    collect :: ArraysR arrs -> arrs -> [Doc]
    collect ArraysRunit         _        = []
    collect ArraysRarray        arr      = [prettyArray arr]
    collect (ArraysRpair r1 r2) (a1, a2) = collect r1 a1 ++ collect r2 a2

prettyArray :: forall dim e. Array dim e -> Doc
prettyArray arr@(Array sh _)
  = hang (text "Array") 2 $ sep [parens $ showDoc (toElt sh :: dim), dataDoc]
  where
    showDoc :: forall a. Show a => a -> Doc
    showDoc = text . show
    l       = toList arr
    dataDoc | length l <= 1000 = showDoc l
            | otherwise        = showDoc (take 1000 l) <+>
                                 text "{truncated at 1000 elements}"

idxToInt :: Idx env t -> Int
idxToInt = go 0
  where
    go :: Int -> Idx env t -> Int
    go !n ZeroIdx       = n
    go !n (SuccIdx idx) = go (n+1) idx

tupleIdxToInt :: TupleIdx t e -> Int
tupleIdxToInt = go 0
  where
    go :: Int -> TupleIdx t e -> Int
    go !n ZeroTupIdx       = n
    go !n (SuccTupIdx idx) = go (n+1) idx

