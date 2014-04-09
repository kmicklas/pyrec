module Pyrec.AST.Check where
-- The AST after checking

import qualified Pyrec.AST as A
import           Pyrec.AST.Parse   (Loc, BindT, BindN)
import           Pyrec.AST.Desugar (Type)

data Id
  = Bound Loc String
  | NotMutable Loc String
  | Unbound String
  deriving (Eq, Show)

data Expr
  = E Loc Type (A.Expr BindT BindN Id Expr Type)
  deriving (Eq, Show)
