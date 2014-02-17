module Pyrec.AST where

-- Base AST

data Expr bd id ex ty
  = Num Double
  | Str String
  | Ident id
  | Fun [bd] ex
  | Let   (Decl bd id ex) ex
  | Graph [Decl bd id ex] ex
  | App ex [ex]
  | Assign id ex
  | Cases ty ex [Case bd id ex]
  | Try ex bd ex
  deriving Eq

data Type id ty
  = TAny
  | TNum
  | TStr
  | TIdent id
  | TFun [ty] ty
  deriving Eq

data DefType
  = Val | Var
  deriving Eq

data Decl bd id ex
  = Def DefType bd ex
  | Data id [Variant bd id]
  deriving Eq

data Case bd id ex
  = Case (Pattern bd id) ex
  deriving Eq

data Pattern bd id
  = Constr id (Maybe [Pattern bd id])
  | Binding bd
  deriving Eq

data Variant bd id
  = Variant id (Maybe [bd])
  deriving Eq
