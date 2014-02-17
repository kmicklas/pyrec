module Pyrec.AST.Parse where
-- The AST after parsing

import qualified Pyrec.AST as A

type Loc = Int
type Id = String

data Bind
  = B Loc String Type
  deriving Eq

data Type
  = T (A.Type Id Type)
  | TUnknown
  | TError TypeError
  deriving Eq

data TypeError
  = TypeMismatch {expected :: Type, got :: Type}
  | ExtraArg
  deriving Eq

data Expr = E Loc Type (A.Expr Bind Id Expr Type)
