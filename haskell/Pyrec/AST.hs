{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Pyrec.AST where
-- The AST after parsing

import Data.Foldable
import Data.Traversable

import Text.Parsec.Pos

data Node a
  = Node SourcePos a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type Id = Node String

data Bind id
  = Bind id (Maybe (Node Type))
  deriving (Eq, Ord)

data Module
  = Module (Node Provide) [Node Import] Block
  deriving (Eq, Ord)

data Provide
  = NoProvide
  | ProvideAll
  | ProvideExpr (Node Expr)
  deriving (Eq, Ord)

data Import
  = Import ModuleName
  | ImportQualified ModuleName Id
  deriving (Eq, Ord)

data ModuleName
  = Named Id
  | File String
  deriving (Eq, Ord)

newtype Block
  = Statements [Node Statement]
  deriving (Eq, Ord)

data Statement
  = ExprStmt (Node Expr)
  | LetStmt (Let Id)
  | VarStmt (Let Id)
  | AssignStmt Id (Node Expr)
  | Graph Block
  | FunStmt (Maybe [Id]) Id (Maybe [Bind Id]) (Maybe (Node Type)) Block
  | Data Id (Maybe [Id]) [Variant]
  deriving (Eq, Ord)

data Variant
  = Variant Id [Bind Id]
  deriving (Eq, Ord)

data Let id
  = Let (Bind id) (Node Expr)
  deriving (Eq, Ord)

data Type
  = TIdent Id
  | TFun   [Type] Type
  | TParam [Id]   Type
  | TObject [Bind Id]
  deriving (Eq, Ord)

data Expr
  = Num Double
  | Str String
  | Ident Id
  | Obj [Node Field]

  | App  (Node Expr) [Node Expr]
  | AppT (Node Expr) [Node Type]
  | UnOp String (Node Expr)
  | BinOp String (Node Expr) (Node Expr)

  | Fun (Maybe [Id]) (Maybe [Bind Id]) (Maybe (Node Type)) Block
  | Block Block

  | TypeConstraint (Node Expr) (Node Type)
  deriving (Eq, Ord)

data Field
  = Immut (Let Key)
  | Mut   (Let Key)
  deriving (Eq, Ord)

data Key
  = Name Id
  | Dynamic (Node Expr)
  deriving (Eq, Ord)
