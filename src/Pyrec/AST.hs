{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Pyrec.AST where
-- The AST after parsing

import Data.Foldable
import Data.Traversable

import Text.Parsec.Pos

data Node a
  = Node SourcePos a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

type Id = Node String

data Bind id
  = Bind id (Maybe (Node Type))
  deriving (Eq, Show, Ord)

data Module
  = Module (Node Provide) [Node Import] Block
  deriving (Eq, Show, Ord)

data Provide
  = NoProvide
  | ProvideAll
  | ProvideExpr (Node Expr)
  deriving (Eq, Show, Ord)

data Import
  = Import ModuleName
  | ImportQualified ModuleName Id
  deriving (Eq, Show, Ord)

data ModuleName
  = Named Id
  | File String
  deriving (Eq, Show, Ord)

newtype Block
  = Statements [Node Expr]
  deriving (Eq, Show, Ord)

data Variant
  = Variant Id [Bind Id]
  deriving (Eq, Show, Ord)

data Let id
  = Let (Bind id) (Node Expr)
  deriving (Eq, Show, Ord)

data Type
  = TIdent Id
  | TFun   [Type] Type
  | TParam [Id]   Type
  | TObject [Bind Id]
  deriving (Eq, Show, Ord)

data Expr
  = LetStmt (Let Id)
  | VarStmt (Let Id)
  | AssignStmt Id (Node Expr)
  | Graph Block
  | Data Id (Maybe [Id]) [Variant]

  | Num Double
  | Str String
  | Ident Id
  | Obj [Node Field]

  | App  (Node Expr) [Node Expr]
  | AppT (Node Expr) [Node Type]
  | Dot  (Node Expr) Key
  | UnOp String (Node Expr)
  | BinOp (Node Expr) [(String, Node Expr)]

  | Fun (Maybe [Id]) (Maybe Id) (Maybe [Bind Id]) (Maybe (Node Type)) Block
  | Block Block
  | If [Branch] (Maybe Block)
  | Cases (Maybe (Node Type)) (Node Expr) [Case]

  | TypeConstraint (Node Expr) (Node Type)
  deriving (Eq, Show, Ord)

data Branch
  = Branch (Node Expr) Block
  deriving (Eq, Show, Ord)

data Case
  = Case Id (Maybe [Bind Id]) Block
  | Else Block
  deriving (Eq, Show, Ord)

data Field
  = Immut (Let Key)
  | Mut   (Let Key)
  deriving (Eq, Show, Ord)

data Key
  = Name Id
  | Dynamic (Node Expr)
  deriving (Eq, Show, Ord)
