module Pyrec.AST where

type Loc = Int

data Id
  = Name Loc String
  | Gen String
  deriving (Eq, Show, Ord)

data LExpr = At Loc Expr Expr deriving Eq
data TExpr = As Expr Expr deriving Eq

type Expr = PExpr LExpr

data PExpr e
  = Def (Def e) e
  | Assign Id e
  | Cases e [Case e]
  | Try e e
  | Fun [Id] e
  | App e [e]
  | ParamApp e [e]
  | Ident Id
  | Number Double
  | Str String
  | Error String e
  | TType
  | TParamType [Id]
  | TUnknown
  | TAny
  | TNum
  | TStr
  | TFun [Id] [Bind e] e
  | TParam [Id] e
  deriving Eq

data Def e
  = Let (Decl e)
  | Graph [Decl e]
  deriving Eq

data Decl e
  = Val (Bind e) e
  | Var (Bind e) e
  | Data Id [Id] [Variant e]
  deriving Eq

data Bind e = Bind Id e deriving Eq

data Case e = Case (Pattern e) e deriving Eq

data Pattern e
  = Constr Id (Maybe [Pattern e])
  | Binding (Bind e)
  deriving Eq

data Variant e = Variant Id (Maybe [Bind e]) deriving Eq
