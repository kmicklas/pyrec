module Pyrec.AST.Parse where
-- The AST after parsing

import qualified Pyrec.AST as A

type Loc = Int
type Id = String

data BindT
  = BT Loc String Type
  deriving (Eq, Show)

data BindN
  = BN Loc String Type
  deriving (Eq, Show)

data Type
  = T (A.Type Id Type)
  deriving (Eq, Show)

data Expr
  = E Loc Type (A.Expr BindT BindN Id Expr Type)
  | Sugar -- TEMP, you flesh it out Kenny
  deriving (Eq, Show)
