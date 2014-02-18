module Pyrec.AST.Core where
-- The AST after inserting runtime errors

import qualified Pyrec.AST as A
import Pyrec.AST.Parse (Loc, Type, Bind, TypeError)

data Id
  = Bound Loc String
  deriving (Eq, Show)

data Expr
  = E Loc Type (A.Expr Bind Id Expr Type)
  | Error ErrorMessage
  deriving (Eq, Show)

data Error
  = UnboundId String
  | MutateVar Loc String
  | TypeError TypeError
  deriving (Eq, Show)

type ErrorMessage = (Loc, Error)