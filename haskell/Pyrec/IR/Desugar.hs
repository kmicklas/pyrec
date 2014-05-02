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
  = T (IR.Type BindN Id Type)
  | TUnknown
  | PartialObj (Map IR.FieldName Type)
  | TError TypeError
  deriving (Eq, Show)

-- the type checking algorithm insists we use
-- the same Type adt before and after checking
data TypeError
  = TypeMismatch {expected :: Type, got :: Type}
  | CantCaseAnalyze               { got :: Type}
  deriving (Eq)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  deriving (Eq, Show)

data Error
  = EndBlockWithDef
  | SameLineStatements
  deriving (Eq)

type ErrorMessage = (Loc, Error)

instance Show TypeError where
  show e = case e of
    TypeMismatch exp got -> "Expected " ++ show exp ++ ", got " ++ show got
    CantCaseAnalyze ty   -> "Cannot use \"Cases ... end\" to deconstruct " ++ show ty

instance Show Error where
  show e = case e of
    EndBlockWithDef    -> "The last element in a block must be an expression"
    SameLineStatements -> "Two statements should never be put on the same line"
