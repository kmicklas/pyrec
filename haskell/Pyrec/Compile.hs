module Pyrec.Compile where

import Pyrec.AST
import Pyrec.AST.Core as R
import Pyrec.SSA

temp :: R.Loc -> Id

compile :: R.Expr -> (Module, [Bind], Id)
compile (R.E l _ e) = case e of
  
  Num n -> (singleton id (Num n), [Bind id $ Atomic $ Bound id], id)
    where id = temp l
