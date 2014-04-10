module Pyrec.IR.Check where
-- The IR after checking

import qualified Pyrec.IR as IR
import           Pyrec.IR.Parse   (Loc, BindN)
import           Pyrec.IR.Desugar (BindT, Type)

data Id
  = Bound IR.DefType Loc String
  | Unbound String
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  deriving (Eq, Show)
