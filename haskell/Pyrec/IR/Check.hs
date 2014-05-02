module Pyrec.IR.Check where
-- The IR after checking

import qualified Pyrec.IR as IR
import           Pyrec.IR.Desugar (Loc, BindN, BindT, Type)

data Id
  = Bound   {_dt :: IR.DefType, _l :: Loc, getId :: String }
  | Unbound                               {getId :: String }
  deriving (Eq)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  deriving (Eq)
