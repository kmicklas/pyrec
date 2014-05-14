module Pyrec.CPS where

import Data.Word
import Pyrec.Misc

data Name
  = Name String Unique
  | Gen Word
  deriving (Show, Eq, Ord)

data Val
  = Var Name
  | Num Double
  | Str String
  | Cont Name Expr
  deriving (Show, Eq)

data Expr
  = App Val [Val] (Val, Val)
  | Continue Val Val
  | Fix [Fun] Expr
  deriving (Show, Eq)

data Fun
  = Fun Name [Name] (Name, Name) Expr
  deriving (Show, Eq)
