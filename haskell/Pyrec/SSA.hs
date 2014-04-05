module Pyrec.SSA where

import qualified Data.Map as M
import           Data.Map (Map)

type Id = String

type Module = Map Id Constant

data Constant
  = Num Double
  | Str String
  | Fun [Id] [Block]
  | Eliminator [(Id, Maybe Int)] -- constructor name and number of arguments
  | Constructor Id (Maybe Int) -- name and number of arguments
  | Extern String Int -- name and number of arguments
  deriving (Show, Eq)

data Block = Block [Statement] Jump deriving (Show, Eq)

data Statement
  = Bind Id Val
  | Assign Id Atom
  deriving (Show, Eq)

data Val
  = Atomic Atom
  | Phi [Id]
  | Call Atom [Atom]
  | Alloca
  | Load Id
  deriving (Show, Eq)

data Atom
  = Const Id
  | Bound Id
  deriving (Show, Eq)

data Jump
  = Return Atom
  deriving (Show, Eq)
