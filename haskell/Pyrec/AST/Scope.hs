module Pyrec.AST.Scope where
-- The AST after Scope Checking

import qualified Pyrec.AST as A
import Pyrec.AST.Parse (Loc, Bind)

data ID = Bound String
        | Unbound String 
        deriving Eq

data Type = T (A.Type ID Type)
          | TError ErrorMessage ty ty
          deriving Eq

data Expr = E Loc Type (A.Expr Bind ID Expr Type)
