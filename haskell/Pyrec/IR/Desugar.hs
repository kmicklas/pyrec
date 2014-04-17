module Pyrec.IR.Desugar where
-- The IR after desugaring

import Text.Parsec.Pos

import qualified Pyrec.IR as IR

type Id  = String
type Loc = SourcePos

data BindT
  = BT Loc String Type
  deriving (Eq, Show)

data BindN
  = BN Loc String
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  deriving (Eq, Show)
