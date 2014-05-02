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
  deriving (Eq, Show)

data Error
  = EndBlockWithDef
  | SameLineStatements
  deriving (Eq)

type ErrorMessage = Message Error

instance Show Type where
  show t = case t of
    (T t)               -> show t
    TUnknown            -> "?"
    (PartialObj fields) -> "{" ++ (intercalate ", " $ map f $ M.toList fields) ++ "}"
      where f (k,v) = show k ++ " : " ++ show v
    (TError e)          -> show e

instance Show TypeError where
  show e = case e of
    TypeMismatch exp got -> "Expected " ++ show exp ++ ", got " ++ show got
    CantCaseAnalyze ty   -> "Cannot use \"Cases ... end\" to deconstruct " ++ show ty

instance Show Error where
  show e = case e of
    EndBlockWithDef    -> "The last element in a block must be an expression"
    SameLineStatements -> "Two statements should never be put on the same line"
