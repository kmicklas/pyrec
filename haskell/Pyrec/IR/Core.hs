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
  deriving (Eq, Show)

-- | We use this to find errors in dead code too
data ExprWithErrors i
  = EE Loc Type [Error] (IR.Expr BindT BindN i Type (ExprWithErrors i))
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Error [ErrorMessage]
  deriving (Eq, Show)

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

instance Show Error where
  show e = case e of
    Earlier    error     -> show error

    UnboundId  ident     -> show ident ++ " is unbound"
    MutateVar  loc ident -> "cannot mutate non-variable " ++ ident ++ ", bound at " ++ show loc

    DupIdent dt loc iden -> sentance1 ++ " one of them is bound at " ++ show loc ++ "."
      where sentance1 = case dt of
              Pattern -> "pattern binds multiple identifiers named " ++ show iden ++ "."
              Constr  -> "type has multiple variants named "         ++ show iden ++ "."
              Graph   -> "graph has multiple declerations named "    ++ show iden ++ "."

    TypeError ty err -> show err ++ " in " ++ show ty

instance Show TypeError where
  show error = case error of
    TEEarlier terror    -> show terror

    AmbiguousType        -> "ambiguous type ecountered"
    PartialObj fields    -> "ambiguous object type encountered with fields " ++ show fields
