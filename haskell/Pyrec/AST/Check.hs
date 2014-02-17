module Pyrec.AST.Check where
-- The AST after Checking

import qualified Pyrec.AST as A
import Pyrec.AST.Parse (Loc, Type, Bind)

data Id
  = Bound Loc String
  deriving (Eq, Show)

data Expr
  = E Loc Type (A.Expr Bind Id Expr Type)
  | Error ErrorMessage Expr
  deriving (Eq, Show)

data ErrorMessage
  = Unbound    String
  | MutateVal  Id
  | TypeAsExpr Id
  deriving (Eq, Show)

bad :: Loc
bad = 0
