{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

module Pyrec.IR.ScopeCheck where
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

data BindT
  = BT Unique String Type
  deriving (Eq, Show)

data Id
  = Bound {- IR.DefType -} Unique String
  deriving (Eq, Show)

-- used as constraint type later on
data Type
  = T (IR.Type BindN Id Type)
  | TUnknown
  | PartialObj (Map IR.FieldName Type)
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
