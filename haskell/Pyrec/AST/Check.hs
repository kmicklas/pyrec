module Pyrec.AST.Check where
-- The AST after Checking

import qualified Pyrec.AST as A
import Pyrec.AST.Parse (Loc, Type, Bind,)

data Id
  = Bound Loc String
  | NotMutable Loc String
  | Unbound String
  deriving (Eq, Show)

data Expr
  = E Loc Type (A.Expr Bind Id Expr Type)
  deriving (Eq, Show)
