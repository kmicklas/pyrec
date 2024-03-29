{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

module Pyrec.IR where
-- Abstract IR

import Control.Applicative

import Data.Foldable
import Data.Traversable
import Data.Map (Map)
import Data.List

#ifdef TEST
import Test.Hspec
import Test.Hspec.QuickCheck    (prop)
import Test.QuickCheck          hiding ((.&.))

import Pyrec.TestMisc
#endif

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
  | Fun  [bt] ex -- \lambda x : Type . Expr
  | FunT [bn] ex -- \Lambda A        . Expr

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
  = TIdent id
  | TFun [ty] ty   -- (A, B, C) -> R
  | TParam [bn] ty -- <A, B, C> -> T

  | TType -- not used in System F, but useful for errors now and future extensions later...

  | TObject (Map FieldName ty)
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

#ifdef TEST
{-!
deriving instance Arbitrary Expr
deriving instance Arbitrary Type
deriving instance Arbitrary DefType
deriving instance Arbitrary Decl
deriving instance Arbitrary Case
deriving instance Arbitrary Pattern
deriving instance Arbitrary Variant
!-}
#endif
