{-# LANGUAGE FlexibleInstances #-}
module Pyrec.PrettyPrint where

import           Prelude

import           Control.Applicative

import           Data.List
import qualified Data.Map            as M
import           Data.Map                   (Map)
import           Data.Maybe                 (fromMaybe)

import           Text.Parsec.Pos

import           Pyrec.Misc

import           Pyrec.IR

import qualified Pyrec.IR.Desugar    as D
import qualified Pyrec.IR.Check      as C
import qualified Pyrec.IR.Core       as R

import           Pyrec.Error

class PrettyPrint a where
  pp :: a -> String

parenList :: [String] -> String
parenList l = "("  ++ intercalate ", " l ++ ")"

angleList :: [String] -> String
angleList l = "<"  ++ intercalate ", " l ++ ">"

curlyList :: [String] -> String
curlyList l = "{"  ++ intercalate ", " l ++ "}"

block :: String -> String
block s = ":\n" ++ (unlines $ ("  " ++) <$> lines s)

opt :: Maybe String -> String
opt Nothing  = ""
opt (Just i) = i

optArgs :: (PrettyPrint p) => Maybe [p] -> String
optArgs Nothing   = ""
optArgs (Just as) = parenList $ pp <$> as

instance PrettyPrint String where
  pp s = s

instance PrettyPrint e => PrettyPrint (Message e) where
  pp (Msg loc error) = show loc ++ ":\n" ++ pp error

instance (PrettyPrint bt, PrettyPrint bn, PrettyPrint id,
          PrettyPrint ty, PrettyPrint ex) =>
         PrettyPrint (Expr bt bn id ty ex) where
  pp = \case
    Num n     -> show n
    Str s     -> show s
    Ident i   -> pp i
    Fun  bs e -> "fun" ++ parenList (pp <$> bs) ++ block (pp e) ++ "end"
    FunT bs e -> "fun" ++ angleList (pp <$> bs) ++ block (pp e) ++ "end"

    App  f args -> pp f ++ parenList (pp <$> args)
    AppT f args -> pp f ++ angleList (pp <$> args)

    Cases t e cs -> "cases(" ++ pp t ++ ") " ++ pp e ++ (block $ unlines $ pp <$> cs) ++ "end"

    Try e b ex     -> "try" ++ block (pp e) ++ "except " ++ pp b ++ block (pp ex) ++ "end"
    EmptyObject    -> "{}"
    Extend obj f v -> pp obj ++ ".{" ++ f ++ ": " ++ pp v ++ "}"
    Access obj f   -> pp obj ++ "." ++ f

    Let   d  e   -> undefined
    Graph ds e   -> undefined
    Assign _ _   -> undefined

instance (PrettyPrint bn, PrettyPrint id, PrettyPrint ty) =>
         PrettyPrint (Type bn id ty) where
  pp = \case
    TIdent id   -> pp id
    TFun ps t   -> parenList (pp <$> ps) ++ " -> " ++ pp t
    TParam ps t -> angleList (pp <$> ps) ++ " -> " ++ pp t
    TType       -> "Type"
    TObject fs  -> curlyList $ for (M.toList fs) $ \ (f, t) -> f ++ ": " ++ pp t

instance PrettyPrint DefType where
  pp Val = " = "
  pp Var = " := "

instance (PrettyPrint bt, PrettyPrint bn, PrettyPrint ex) =>
         PrettyPrint (Decl bt bn ex) where
  pp = \case
    Def dt b e -> pp b ++ pp dt ++ pp e
    Data b vs  -> "data " ++ pp b ++ (block $ unlines $ pp <$> vs) ++ "end"

instance (PrettyPrint bt, PrettyPrint bn, PrettyPrint ex) =>
         PrettyPrint (Case bt bn ex) where
  pp (Case p e) = "| " ++ pp p ++ " => " ++ pp e

instance (PrettyPrint bt, PrettyPrint bn) =>
         PrettyPrint (Pattern bt bn) where
  pp (Constr b ps) = pp b ++ optArgs ps

instance (PrettyPrint bt, PrettyPrint bn) =>
         PrettyPrint (Variant bt bn) where
  pp (Variant b args) = pp b ++ optArgs args

instance PrettyPrint D.Error where
  pp = \case
    D.EndBlockWithDef    -> "The last element in a block must be an expression"
    D.SameLineStatements -> "Two statements should never be put on the same line"

instance PrettyPrint R.Error where
  pp = \case
    R.Earlier    error     -> pp error

    R.UnboundId  ident     -> show ident ++ " is unbound"
    R.MutateVar  loc ident -> "cannot mutate non-variable " ++ ident ++ ", bound at " ++ show loc

    R.DupIdent dt loc iden -> sentance1 ++ " one of them is bound at " ++ show loc ++ "."
      where sentance1 = case dt of
              R.Pattern -> "pattern binds multiple identifiers named " ++ iden ++ "."
              R.Constr  -> "type has multiple variants named "         ++ iden ++ "."
              R.Graph   -> "graph has multiple declerations named "    ++ iden ++ "."

    R.TypeError ty err -> pp err ++ " in " ++ pp ty

instance PrettyPrint D.BindT where
  pp (D.BT _ n t) = n ++ " :: " ++ pp t

instance PrettyPrint D.BindN where
  pp (D.BN _ n) = n

instance PrettyPrint D.Type where
  pp = \case
    D.T t -> pp t
    D.TUnknown -> "?"
    D.PartialObj fs -> curlyList $ (++ ["..."]) $
      for (M.toList fs) $ \ (f, t) -> f ++ ": " ++ pp t

instance PrettyPrint D.TypeError where
  pp = \case
    D.TypeMismatch exp got -> "Expected " ++ pp exp ++ ", got " ++ pp got
    D.CantCaseAnalyze ty   -> "Cannot use \"Cases ... end\" to deconstruct " ++ pp ty

instance PrettyPrint R.TypeError where
  pp = \case
    R.TEEarlier terror    -> pp terror

    R.AmbiguousType        -> "ambiguous type ecountered"
    R.PartialObj fields    -> "ambiguous object type encountered with fields "
                              ++ (curlyList $ (++ ["..."]) $ for (M.toList fields) $
                                \ (f, t) -> f ++ ": " ++ pp t)
