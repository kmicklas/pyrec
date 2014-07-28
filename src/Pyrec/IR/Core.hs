module Pyrec.IR.Core where
-- The IR after inserting runtime errors

import Control.Applicative

import Data.Map (Map)

import           Pyrec.Misc
import           Pyrec.Error

import qualified Pyrec.IR            as IR
import           Pyrec.IR.Desugar          (BindN)
import qualified Pyrec.IR.Desugar    as D
import qualified Pyrec.IR.ScopeCheck as SC
import           Pyrec.IR.ScopeCheck       (Id)
import qualified Pyrec.IR.TypeCheck  as TC
import           Pyrec.IR.TypeCheck        (BindT, Type, TypeError)

data Expr
  = E Unique Type (IR.Expr BindT BindN Id Type Expr)
--  | Error [ErrorMessage]
  deriving (Eq, Show)

data DupType
  = Graph
  | Pattern
  | Constr
  deriving (Eq, Show)

data Error
  = Earlier D.Error

  | UnboundId String
  | MutateVar Unique String

  | DupIdent DupType Unique String

  | TypeError Type TypeError
  deriving (Eq, Show)

type ErrorMessage = Message Error
