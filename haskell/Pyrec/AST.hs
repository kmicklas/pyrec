{-# LANGUAGE DeriveFunctor #-}

module Pyrec.AST where
-- The AST after parsing

import Text.Parsec.Pos

data Node a
  = Node SourcePos a
  deriving (Eq, Show, Ord, Functor)

data Module
  = Module (Node Provide) [Node Import] (Node Expr)
  deriving (Eq, Show, Ord)

data Provide
  = NoProvide
  | ProvideAll
  | ProvideExpr (Node Expr)
  deriving (Eq, Show, Ord)

data Import
  = Import ModuleName
  | ImportQualified ModuleName String
  deriving (Eq, Show, Ord)

data ModuleName
  = Named String
  | File String
  deriving (Eq, Show, Ord)

data Expr
  = Num Double
  deriving (Eq, Show, Ord)
