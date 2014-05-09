{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

module Pyrec.Misc where

import Control.Applicative

import Data.Word

import Data.Hashable
import Data.Foldable
import Data.Traversable hiding (for)

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

-- | should be pretty self explanatory
for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- | like zipWith, but aborts if the lists are not the same length
map2S :: (a -> b -> r) -> ([a] -> [b] -> Maybe [r])
map2S _ [] []         = Just []
map2S f (a:as) (b:bs) = (f a b :) <$> map2S f as bs
map2S _ _ _           = Nothing

showLoc :: SourcePos -> String
showLoc p = show $ abs $ hash (sourceName p,
                               sourceLine p,
                               sourceColumn p)

infixl 4 <$$>
(<$$>) :: (Functor f, Functor f1) =>
          (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = fmap . fmap

infixl 4 <$$$>
(<$$$>) :: (Functor f, Functor f1, Functor f2) =>
           (a -> b) -> f (f1 (f2 a)) -> f (f1 (f2 b))
(<$$$>) = fmap . fmap . fmap

infixr 9 <.>
(<.>) :: Functor f =>
         (b -> c) -> (a -> f b) -> a -> f c
a <.> b = fmap a . b

infixr 9 <..>
(<..>) :: (Functor f, Functor f1) =>
          (b -> c) -> (a -> f (f1 b)) -> a -> f (f1 c)
a <..> b = (fmap . fmap) a . b

infixr 9 <...>
(<...>) :: (Functor f, Functor f1, Functor f2) =>
           (b -> c) -> (a -> f (f1 (f2 b))) -> a -> f (f1 (f2 c))
a <...> b = (fmap . fmap . fmap) a . b


#ifdef TEST
{-!
deriving instance Arbitrary Unique
!-}
#endif
