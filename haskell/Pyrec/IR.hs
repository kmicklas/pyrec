{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Pyrec.IR where
-- Abstract IR

import Data.Foldable
import Data.Traversable

data Expr
     bt -- Binding with Type
     bn -- Binding with No type
     id -- Identifier
     ty -- Type
     ex -- Expression
  = Num Double
  | Str String
  | Ident id
  | Fun [bt] ex
  | Let   (Decl bt bn ex) ex
  | Graph [Decl bt bn ex] ex
  | App ex [ex]
  | Assign id ex
  | Cases ty ex [Case bt bn ex]
  | Try ex bt ex
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Type id ty
  = TAny
  | TNum
  | TStr
  | TIdent id
  | TFun [ty] ty
  deriving (Eq, Show, Functor, Foldable, Traversable)

data DefType
  = Val
  | Var
  deriving (Eq, Show)

data Decl bt bn ex
  = Def DefType bt ex
  | Data bn [Variant bt bn]
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Case bt bn ex
  = Case (Pattern bt bn) ex
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Pattern bt bn
  = Constr bn (Maybe [Pattern bt bn])
  | Binding bt
  deriving (Eq, Show)

data Variant bt bn
  = Variant bn (Maybe [bt])
  deriving (Eq, Show)
