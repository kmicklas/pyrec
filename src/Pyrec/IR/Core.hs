module Pyrec.IR.Core where
-- The IR after inserting runtime errors

import Control.Applicative

import Data.Map (Map)

import           Pyrec.Error

import qualified Pyrec.IR         as IR
import           Pyrec.IR.Desugar      (Loc, BindN)
import qualified Pyrec.IR.Desugar as D
import           Pyrec.IR.Check        (BindT, Type)
import qualified Pyrec.IR.Check   as C

data Id
  = Bound Loc String
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Error [ErrorMessage]
  deriving (Eq, Show)

data DupType
  = Graph
  | Pattern
  | Constr
  deriving (Eq, Show)

data Error
  = Earlier D.Error

  | UnboundId String
  | MutateVar Loc String

  | DupIdent DupType Loc String

  | TypeError C.Type TypeError
  deriving (Eq, Show)

data TypeError
  = TEEarlier C.TypeError
  | AmbiguousType
  | PartialObj (Map IR.FieldName C.Type)
  deriving (Eq, Show)

type ErrorMessage = Message Error
