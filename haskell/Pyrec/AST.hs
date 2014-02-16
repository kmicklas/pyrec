module Pyrec.AST where

type Loc = Int

data Id
  = Name Loc String
  | Gen String
  | Unresolved String
  deriving (Eq, Show, Ord)

data LExpr = At Loc Type (PExpr LExpr) deriving Eq

data PExpr e
  = Def (Def e) e
  | Assign Id e
  | Cases Type e [Case e]
  | Try e Bind e
  | Fun [Id] e
  | App e [e]
  | Ident Id
  | Number Double
  | Str String
  | Error ErrorMessage e
  deriving Eq

data Type
  = TUnknown
  | TAny
  | TNum
  | TStr
  | TIdent Id
  | TFun [Type] Type
  | TError ErrorMessage Type
  deriving Eq

data Def e
  = Let (Decl e)
  | Graph [Decl e]
  deriving Eq

data Decl e
  = Val Bind e
  | Var Bind e
  | Data Id [Variant]
  deriving Eq

data Bind = Bind Id Type deriving Eq

data Case e = Case Pattern e deriving Eq

data Pattern
  = Constr Id (Maybe [Pattern])
  | Binding Bind
  deriving Eq

data Variant = Variant Id (Maybe [Bind]) deriving Eq

data ErrorMessage
  = Unbound String
  deriving Eq
