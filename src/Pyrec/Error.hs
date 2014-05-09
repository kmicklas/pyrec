{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

module Pyrec.Error where
-- some data types used for errors, warnings, and their messages

import Data.Word
import Data.Foldable
import Data.Traversable

import Text.Parsec.Pos

#ifdef TEST
import Test.Hspec
import Test.Hspec.QuickCheck    (prop)
import Test.QuickCheck          hiding ((.&.))

import Pyrec.TestMisc
#endif

data Unique
  = Intrinsic
  | User SourcePos Word
  deriving (Eq, Ord, Show)

data Message error
  = Msg Unique error
  deriving (Eq, Show, Functor, Foldable, Traversable)

#ifdef TEST
{-!
deriving instance Arbitrary Unique
!-}
#endif
