module Pyrec.Misc where

import Control.Applicative
import Data.Hashable
import Text.Parsec.Pos

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

infixr 9 <.>
(<.>) :: Functor f => (a1 -> b) -> (a -> f a1) -> a -> f b
a <.> b = fmap a . b
