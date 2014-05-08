{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

module Pyrec.IR.Desugar where
-- The IR after desugaring

import qualified Data.Map          as M
import           Data.Map                       (Map)
import           Data.List

import           Text.Parsec.Pos

import           Pyrec.Error
import qualified Pyrec.IR          as IR

#ifdef TEST
import Test.Hspec
import Test.Hspec.QuickCheck    (prop)
import Test.QuickCheck          hiding ((.&.))

import Pyrec.TestMisc
#endif

type Id  = String
type Loc = SourcePos

data BindT
  = BT Loc String Type
  deriving (Eq, Show)

data BindN
  = BN Loc String
  deriving (Eq, Show)

data Type
  = T (IR.Type BindN Id Type)
  | TUnknown
  | PartialObj (Map IR.FieldName Type)
  | TError TypeError
  deriving (Eq, Show)

-- the type checking algorithm insists we use
-- the same Type adt before and after checking
data TypeError
  = TypeMismatch {expected :: Type, got :: Type}
  | CantCaseAnalyze               { got :: Type}
  deriving (Eq, Show)

data Expr
  = E Loc Type (IR.Expr BindT BindN Id Type Expr)
  | Constraint Loc Type Expr
  deriving (Eq, Show)

data Error
  = EndBlockWithDef
  | SameLineStatements
  deriving (Eq, Show)

type ErrorMessage = Message Error

#ifdef TEST
{-!
deriving instance Arbitrary BindT
deriving instance Arbitrary BindN
deriving instance Arbitrary Type
deriving instance Arbitrary TypeError
deriving instance Arbitrary Expr
deriving instance Arbitrary Error
!-}
#endif
