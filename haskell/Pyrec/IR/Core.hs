module Pyrec.IR.Core where
-- The IR after inserting runtime errors

import qualified Pyrec.IR as IR
import           Pyrec.IR.Desugar (Loc, BindN, BindT, Type, TypeError)
import           Pyrec.Error

data Id
  = Bound Loc String
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Error ErrorMessage
  deriving (Eq, Show)
