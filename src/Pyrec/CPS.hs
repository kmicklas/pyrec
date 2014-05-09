module Pyrec.CPS where

import Data.Word
import Text.Parsec.Pos

data Name
  = Name String SourcePos
  | Gen Word
  deriving (Show, Eq)

data Val
  = Var Name
  | Num Double
  | Str String
  deriving (Show, Eq)

data Expr
  = App Val [Val] (Val, Val)
  | Fix [Fun] Expr
  deriving (Show, Eq)

data Fun
  = Fun Name [Name] (Name, Name) Expr
  deriving (Show, Eq)
