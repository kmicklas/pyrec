module Pyrec.SSA where

import qualified Data.Map as M
import           Data.Map (Map)

type Id = String

type Module = Map Id Constant

data Constant
  = Num Double
  | Str String
  | Fun [Id] [Block]
  | Extern String Int -- number of arguments

data Block = Block [Bind] Jump

data Bind = Bind Id Val

data Val
  = Atomic Atom
  | Phi [Id]
  | Call [Atom]

data Atom
  = Const Id
  | Bound Id

data Jump
  = Return Atom
