{-# LANGUAGE DeriveFunctor #-}

module Pyrec.AST where
-- The AST after parsing

import Text.Parsec.Pos

data Node a
  = Node SourcePos a
  deriving (Eq, Show, Ord, Functor)

type Id = Node String

data Bind
  = Bind Id (Maybe (Node Type))
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

type Block = [Node Statement]

data Statement
  = ExprStmt (Node Expr)
  | LetStmt Let
  | Graph Block
  | Data Id (Maybe [Id]) [Variant]
  deriving (Eq, Show, Ord)

data Variant
  = Variant Id [Bind]
  deriving (Eq, Show, Ord)

data Let
  = Let Bind (Node Expr)
  deriving (Eq, Show, Ord)

data Type
  = TId Id
  deriving (Eq, Show, Ord)

data Expr
  = Num Double
  | Id Id
  deriving (Eq, Show, Ord)
