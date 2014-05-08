module Pyrec.TestMisc where

import           Control.Applicative

import qualified Data.Map              as M
import           Data.Map              (Map)

import           Text.Parsec.Pos

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       hiding ((.&.))

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = M.fromList <$> arbitrary {-[(k, v)]-}

instance Arbitrary SourcePos where
  arbitrary = newPos <$> arbitrary <*> arbitrary <*> arbitrary