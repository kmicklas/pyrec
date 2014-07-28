module Pyrec.IR.TypeCheck where
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

-- passed deeper into the AST to restrict types
type CType = SC.Type
-- returned after a term is typechecked
type RType = Type

data Expr
  = E Unique Type (IR.Expr BindT BindN Id Type Expr)
-- | TypeError Unique TypeError Expr
  deriving (Eq, Show)

data BindT
  = BT Unique String Type
  deriving (Eq, Show)

data Type
  = T (IR.Type BindN Id Type)
  deriving (Eq, Show)

data TypeError
  = TypeMismatch Type --expected
  | CantCaseAnalyze
  | AmbiguousType
  deriving (Eq, Show)
