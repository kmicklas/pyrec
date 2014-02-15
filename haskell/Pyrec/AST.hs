module Pyrec.AST where


data PExpr id e
  = Let (Decl id e) e
  | Graph [Decl id e] e
  | Assign id e
  | Cases e [Case id e]
  | Try e e
  | Fun [id] e
  | App e [e]
  | Ident id
  | Number Double
  | Str String
  | Error String e
  | TType
  | TUnknown
  | TAny
  | TNum
  | TStr
  | TFun [id] [Bind id e] e
  deriving (Eq)

data Decl id e
  = Val (Bind id e) e
  | Var (Bind id e) e
  | Data id [id] [Variant id e]
  deriving (Eq)
           
data Bind id e = Bind id e
               deriving (Eq)

data Case id e = Case (Pattern id e) e
               deriving (Eq)

data Pattern id e
  = Constr id (Maybe [Pattern id e])
  | Binding (Bind id e)
  deriving (Eq)

data Variant id e = Variant id (Maybe [Bind id e])
                  deriving (Eq)