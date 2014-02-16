module Pyrec.AST where

type Loc = Int

data Id
  = Name Loc String
  | Gen String
  | Unresolved String
  deriving (Eq, Show, Ord)

data LExpr = At Loc Type (PExpr LExpr) deriving Eq

data PExpr exp
  = Def (Def exp) exp
  | Assign Id exp
  | Cases Type exp [Case exp]
  | Try exp Bind exp
  | Fun [Id] exp
  | App exp [exp]
  | Ident Id
  | Num Double
  | Str String
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
