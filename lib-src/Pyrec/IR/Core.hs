module Pyrec.IR.Core where
-- The IR after inserting runtime errors

import Control.Applicative

import Data.Map (Map)

import           Pyrec.Error

import qualified Pyrec.IR         as IR
import           Pyrec.IR.Desugar      (Loc, BindN, BindT, Type)
import qualified Pyrec.IR.Desugar as D
import qualified Pyrec.IR.Check   as C

data Id
  = Bound Loc String
  deriving (Eq)

-- | We use this to find errors in dead code too
data ExprWithErrors i
  = EE Loc Type [Error] (IR.Expr BindT BindN i Type (ExprWithErrors i))
  deriving (Eq)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Error [ErrorMessage]
  deriving (Eq)

data DupType
  = Graph
  | Pattern
  | Constr
  deriving (Eq)

data Error
  = Earlier D.Error

  | UnboundId String
  | MutateVar Loc String

  | DupIdent DupType Loc String

  | TypeError D.Type TypeError
  deriving (Eq)

data TypeError
  = TEEarlier D.TypeError
  | AmbiguousType
  | PartialObj (Map IR.FieldName D.Type)
  deriving (Eq)

type ErrorMessage = Message Error
