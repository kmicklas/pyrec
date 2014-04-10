module Pyrec.IR.Desugar where
-- The IR after desugaring

import qualified Pyrec.IR as IR
import           Pyrec.IR.Parse (Loc, Id, BindN)

data BindT
  = BT Loc String Type
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
  deriving (Eq, Show)
