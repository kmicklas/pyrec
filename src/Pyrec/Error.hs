{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Pyrec.Error where
-- some data types used for errors, warnings, and their messages

import Data.Foldable
import Data.Traversable

import Pyrec.Misc

data Message error
  = Msg Unique error
  deriving (Eq, Show, Functor, Foldable, Traversable)
