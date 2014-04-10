module Pyrec.IR.Core where
-- The IR after inserting runtime errors

import qualified Pyrec.IR as IR
import           Pyrec.IR.Parse   (Loc, BindN)
import           Pyrec.IR.Desugar (BindT, Type, TypeError)
data Id
  = Bound Loc String
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Error ErrorMessage
  deriving (Eq, Show)

data Error
  = UnboundId String
  | MutateVar Loc String
  | TypeError TypeError
  deriving (Eq, Show)

type ErrorMessage = (Loc, Error)
