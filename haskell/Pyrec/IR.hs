{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Pyrec.IR where
-- Abstract IR

import Data.Foldable
import Data.Traversable
import Data.Map (Map)

type FieldName = String

data Expr
     bt -- ^ Binding with Type
     bn -- ^ Binding with No type
     id -- ^ Identifier
     ty -- ^ Type
     ex -- ^ Expression
  = Num Double
  | Str String
  | Ident id
  | Fun  [bt] ex -- λ ident:Type . Expr
  | FunT [bn] ex -- Λ x . Expr

  | Let   (Decl bt bn ex) ex
  | Graph [Decl bt bn ex] ex

  | App  ex [ex]
  | AppT ex [ty]
  | Assign id ex
  | Cases ty ex [Case bt bn ex]

  | Try ex bt ex

  | EmptyObject
  | Extend ex FieldName ex
  | Access ex FieldName
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Type bn id ty
  = TAny
  | TNum
  | TStr
  | TIdent id
  | TFun [ty] ty   -- A → (B → (...))
  | TParam [bn] ty -- ∀a . (∀b . (...))

  | TType -- not used in System F, but useful for errors now and future extensions later...

  | TObject (Map FieldName ty)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Kind
  = KType
  | KFun [Kind] Kind
  deriving (Eq, Show)

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
