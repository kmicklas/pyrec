module Pyrec.CheckSpec where

import           Control.Monad         (mzero)

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       hiding ((.&.))

spec :: Spec
spec = do
  describe "demo" $ do
    it "trying this out" $
      1 == 1 `shouldBe` True
