module Pyrec.IR.Core where
-- The IR after inserting runtime errors

import Data.Map (Map)

import qualified Pyrec.IR as IR
import           Pyrec.IR.Desugar      (Loc, BindN, BindT, Type, TypeError)
import qualified Pyrec.IR.Desugar as D

data Id
  = Bound Loc String
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Error ErrorMessage
  deriving (Eq, Show)

data Error
  = Earlier D.Error

  | UnboundId String
  | MutateVar Loc String
  | TypeError TypeError
  | PartialObj (Map IR.FieldName D.Type)
  deriving (Eq)

type ErrorMessage = (Loc, Error)

instance Show Error where
  show e = case e of
    Earlier    error     -> show error

    UnboundId  ident     -> show ident ++ " is unbound"
    MutateVar  loc ident -> "cannot mutate non-variable " ++ ident ++ ", bound at " ++ show loc
    TypeError  terror    -> show terror
    PartialObj fields    -> "ambiguous type encountered with fields " ++ show fields
