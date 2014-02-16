module Pyrec.AST.Scope where
-- The AST after Scope Checking

import qualified Pyrec.AST as A
import Pyrec.AST.Parse (Loc, Bind, Type)

data ID = Bound Bind Bool
        | Free String
        deriving Eq

data Expr = E Loc Type (A.Expr Bind ID Expr Type)
