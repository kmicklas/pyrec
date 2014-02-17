module Pyrec.AST.Scope where
-- The AST after Scope Checking

import qualified Pyrec.AST as A
import Pyrec.AST.Parse (Loc, Bind, Type, ID)

data Expr = E Loc Type (A.Expr Bind ID Expr Type)
