module Pyrec.Misc where

import Control.Monad

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

