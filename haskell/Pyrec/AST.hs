module Pyrec.AST where

type Loc = Int

data Id
  = Name Loc String
  | Gen String
  | Unresolved String
  deriving (Eq, Show, Ord)

data LExpr = At Loc Type (PExpr LExpr) deriving Eq

data PExpr exp
  = Num Double
  | Str String
  | Ident Id
  | Fun [Id] exp

  | Def (Def exp) exp
  | App exp [exp]
  | Assign Id exp
  | Cases Type exp [Case exp]
  | Try exp Bind exp

  | Error ErrorMessage exp
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

data Def exp
  = Let (Decl exp)
  | Graph [Decl exp]
  deriving Eq

data Decl exp
  = Val Bind exp
  | Var Bind exp
  | Data Id [Variant]
  deriving Eq

data Bind = Bind Id Type deriving Eq

data Case exp = Case Pattern exp deriving Eq

data Pattern
  = Constr Id (Maybe [Pattern])
  | Binding Bind
  deriving Eq

data Variant = Variant Id (Maybe [Bind]) deriving Eq

data ErrorMessage
  = Unbound String
  deriving Eq
