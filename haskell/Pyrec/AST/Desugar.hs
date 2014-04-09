module Pyrec.AST.Desugar where
-- The AST after desugaring

import qualified Pyrec.AST as A
import           Pyrec.AST.Parse (Loc, Id, BindN)

data BindT
  = BT Loc String Type
  deriving (Eq, Show)

data Type
  = T (A.Type Id Type)
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
  = E Loc Type (A.Expr BindT BindN Id Expr Type)
  deriving (Eq, Show)
