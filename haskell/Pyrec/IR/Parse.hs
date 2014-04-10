module Pyrec.IR.Parse where
-- The IR after parsing

import qualified Pyrec.IR as IR

type Loc = Int
type Id = String

data BindT
  = BT Loc String Type
  deriving (Eq, Show)

data BindN
  = BN Loc String
  deriving (Eq, Show)

data Type
  = T (IR.Type Id Type)
  | TSugar
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Sugar -- TEMP, you flesh it out Kenny
  deriving (Eq, Show)
