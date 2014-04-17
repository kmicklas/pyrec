module Pyrec.Error where

import Text.Parsec.Pos

import Pyrec.IR.Desugar (Type)

type ErrorMessage = (SourcePos, Error)

-- the type checking algorithm insists we use
-- the same Type adt before and after checking
data TypeError
  = TypeMismatch {expected :: Type, got :: Type}
  | CantCaseAnalyze               { got :: Type}
  deriving (Eq, Show)

data Error
  = UnboundId String
  | MutateVar SourcePos String
  | TypeError TypeError
  deriving (Eq, Show)
