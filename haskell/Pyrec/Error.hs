{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Pyrec.Error where
-- some data types used for errors, warnings, and their messages

import           Text.Parsec.Pos

import Data.Foldable
import Data.Traversable

data Message error
  = Msg SourcePos error
  deriving (Eq, Functor, Foldable, Traversable)

instance Show error => Show (Message error) where
  show (Msg loc error) =
    show loc ++ ":\n" ++ show error
