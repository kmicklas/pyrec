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
  deriving (Eq, Show)

data Type id ty
  = TAny
  | TNum
  | TStr
  | TIdent id
  | TFun [ty] ty
  deriving (Eq, Show)

data DefType
  = Val
  | Var
  deriving (Eq, Show)

data Decl bd id ex
  = Def DefType bd ex
  | Data id [Variant bd id]
  deriving (Eq, Show)

data Case bd id ex
  = Case (Pattern bd id) ex
  deriving (Eq, Show)

data Pattern bd id
  = Constr id (Maybe [Pattern bd id])
  | Binding bd
  deriving (Eq, Show)

data Variant bd id
  = Variant id (Maybe [bd])
  deriving (Eq, Show)
