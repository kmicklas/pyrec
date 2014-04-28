module Pyrec.IR.Desugar where
-- The IR after desugaring

import Data.Map (Map)

import Text.Parsec.Pos

import qualified Pyrec.IR as IR

type Id  = String
type Loc = SourcePos

data BindT
  = BT Loc String Type
  deriving (Eq, Show)

data BindN
  = BN Loc String
  deriving (Eq, Show)

data Type
  = T (IR.Type Id Type)
  | TUnknown
  | PartialObj (Map IR.FieldName Type)
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

data Error
  = MalformedBlock
  deriving (Eq, Show)

type ErrorMessage = (Loc, Error)
