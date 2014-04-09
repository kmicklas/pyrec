module Pyrec.AST.Check where
-- The AST after checking

import qualified Pyrec.AST as A
import           Pyrec.AST.Parse   (Loc, BindN)
import           Pyrec.AST.Desugar (BindT, Type)

data Id
  = Bound Loc String
  | NotMutable Loc String
  | Unbound String
  deriving (Eq, Show)

data Expr
  = E Loc Type (A.Expr BindT BindN Id Type Expr)
  deriving (Eq, Show)
