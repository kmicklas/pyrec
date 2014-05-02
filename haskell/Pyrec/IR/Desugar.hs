module Pyrec.IR.Desugar where
-- The IR after desugaring

import qualified Data.Map          as M
import           Data.Map                       (Map)
import           Data.List

import           Text.Parsec.Pos

import           Pyrec.Error
import qualified Pyrec.IR          as IR

type Id  = String
type Loc = SourcePos

data BindT
  = BT Loc String Type
  deriving (Eq, Show)

data BindN
  = BN Loc String
  deriving (Eq, Show)

data Type
  = T (IR.Type BindN Id Type)
  | TUnknown
  | PartialObj (Map IR.FieldName Type)
  | TError TypeError
  deriving (Eq)

-- the type checking algorithm insists we use
-- the same Type adt before and after checking
data TypeError
  = TypeMismatch {expected :: Type, got :: Type}
  | CantCaseAnalyze               { got :: Type}
  deriving (Eq)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Constraint Loc Type Expr
  deriving (Eq)

data Error
  = EndBlockWithDef
  | SameLineStatements
  deriving (Eq)

type ErrorMessage = Message Error
