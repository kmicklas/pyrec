{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

module Pyrec.IR.Check where
-- The IR after checking

import qualified Data.Map         as M
import           Data.Map         (Map)

import           Pyrec.Misc

import qualified Pyrec.IR         as IR
import           Pyrec.IR.Desugar (BindN)

#ifdef TEST
import Test.Hspec
import Test.Hspec.QuickCheck    (prop)
import Test.QuickCheck          hiding ((.&.))

import Pyrec.TestMisc
#endif

data Id
  = Bound   {_dt :: IR.DefType, _l :: Unique, getId :: String }
  | Unbound                                  {getId :: String }
  deriving (Eq, Show)

data BindT
  = BT Unique String Type
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
  = E Unique Type (IR.Expr BindT BindN Id Type Expr)
  | Constraint Unique Type Expr
  deriving (Eq, Show)

#ifdef TEST
{-!
deriving instance Arbitrary Id
deriving instance Arbitrary BindT
deriving instance Arbitrary Type
deriving instance Arbitrary TypeError
deriving instance Arbitrary Expr
!-}
#endif
