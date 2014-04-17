module Pyrec.Error where

import Text.Parsec.Pos

import Pyrec.IR.Core (TypeError)

type ErrorMessage = (SourcePos, Error)

data Error
  = UnboundId String
  | MutateVar SourcePos String
  | TypeError TypeError
  deriving (Eq, Show)
