{-# LANGUAGE NoImplicitPrelude, KindSignatures #-}
module Pyrec.AST where

import Prelude (Float, Double, String, Show)

import Data.Eq
import Data.Ord
import Data.Maybe

import Data.Bool
import Data.Int

-- Base ADT

data Expr (bd :: *) (id :: *) (ex :: *) (ty :: *)
  = Num Double
  | Str String
  | Ident id
  | Fun [bd] ex

  | Def (Def bd id ex) ex
  | App ex [ex]
  | Assign id ex
  | Cases ty ex [Case bd id ex]
  | Try ex bd ex

  | Error ErrorMessage ex -- Remove
  deriving Eq

data Type id ty
  = TUnknown  -- Remove
  | TAny
  | TNum
  | TStr
  | TIdent id
  | TFun [ty] ty

  | TError ErrorMessage ty -- Remove
  deriving Eq

data Def bd id ex
  = Let   (Decl bd id ex)
  | Graph [Decl bd id ex]
  deriving Eq

data Decl bd id ex
  = Term Bool bd ex
  | Data id [Variant bd id]
  deriving Eq

data Case bd id ex = Case (Pattern bd id) ex
                 deriving Eq

data Pattern bd id
  = Constr id (Maybe [Pattern bd id])
  | Binding bd
  deriving Eq

data Variant bd id = Variant id (Maybe [bd])
                   deriving Eq

data ErrorMessage = Unbound String
                  deriving Eq

-- After Parsing
{-
-- Id locs are only stored in Bindings
data Bind id = Bind Loc String ty
             deriving Eq

data ParseExpr = PE Loc (Type String) (Expr (Bind Id) String ParseExpr)

-- After Scope Checking

data Bind id = Bind Loc String ty
             deriving Eq

data ParseExpr = PE Loc (Type String) (Expr (Bind Id) String ParseExpr)


data ErrorMessage = Unbound String
  deriving Eq

type Loc = Int

data Id
  = Name Loc String
  | Gen String
  | Unresolved String
  deriving (Eq, Show, Ord)

data LExpr = At Loc (Type Id) (Expr Bind Id LExpr) deriving Eq
-}