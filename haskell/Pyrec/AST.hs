{-# LANGUAGE NoImplicitPrelude #-}
module Pyrec.AST where

import Prelude (Float, Double, String)

import Data.Eq
import Data.Maybe

data PExpr id expr
  = Let (Decl id expr) expr
  | Graph [Decl id expr] expr
  | Assign id expr
  | Cases expr [Case id expr]
  | Try expr expr
  | Fun [id] expr
  | App expr [expr]
  | Ident id
  | Number Double
  | Str String

--  | Error String expr

  | TType
  | TUnknown
  | TAny
  | TNum
  | TStr
  | TFun [id] [Bind id expr] expr
  deriving (Eq)

data Decl id expr
  = Val (Bind id expr) --expr
  | Var (Bind id expr) --expr
  | Data id [id] [Variant id expr]
  deriving (Eq)

data Bind id expr = Bind id expr
                  deriving (Eq)

data Case id expr = Case (Pattern id expr) expr
                  deriving (Eq)

data Pattern id expr
  = Constr id (Maybe [Pattern id expr])
  | Binding (Bind id expr)
  deriving (Eq)

data Variant id expr = Variant id (Maybe [Bind id expr])
                  deriving (Eq)
