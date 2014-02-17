module Pyrec.AST.Check where
-- The AST after Checking

import qualified Pyrec.AST as A
import Pyrec.AST.Parse (Loc, Type, Bind)

data Id
  = Bound Loc String
  deriving Eq

data Expr = E Loc Type (A.Expr Bind Id Expr Type)
          | Error ErrorMessage Expr

data ErrorMessage
  = Unbound    String
  | MutateVal  Id
  | DataAsExpr Id
  | TypeError  Type Type
  deriving Eq

bad :: Loc
bad = 0
