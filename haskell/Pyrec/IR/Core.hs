module Pyrec.IR.Core where
-- The IR after inserting runtime errors

import qualified Pyrec.IR as IR
import           Pyrec.IR.Desugar (Loc, BindN, BindT, Type, TypeError)

data Id
  = Bound Loc String
  deriving (Eq, Show)

data Type
  = T (IR.Type Id Type)
  | TUnknown
  | TError TypeError
  deriving (Eq, Show)

-- the type checking algorithm insists we use
-- the same Type adt before and after checking
data TypeError
  = TypeMismatch {expected :: Type, got :: Type}
  | CantCaseAnalyze               { got :: Type}
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Error ErrorMessage
  deriving (Eq, Show)
