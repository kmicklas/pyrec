{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

module Pyrec.IR.Desugar where
-- The IR after desugaring

import qualified Data.Map          as M
import           Data.Map                       (Map)
import           Data.List

import           Pyrec.Misc
import           Pyrec.Error

import qualified Pyrec.IR          as IR

#ifdef TEST
import Test.Hspec
import Test.Hspec.QuickCheck    (prop)
import Test.QuickCheck          hiding ((.&.))

import Pyrec.TestMisc
#endif

type Id = String

data BindT
  = BT Unique String Type
  deriving (Eq, Show)

data BindN
  = BN Unique String
  deriving (Eq, Show)

data Type
  = T (IR.Type BindN Id Type)
  | TUnknown
  deriving (Eq, Show)

data Expr
  = E Unique Type (IR.Expr BindT BindN Id Type Expr)
  | Constraint Unique Type Expr
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
deriving instance Arbitrary Expr
deriving instance Arbitrary Error
!-}
#endif
