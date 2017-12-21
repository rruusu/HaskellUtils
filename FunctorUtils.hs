module FunctorUtils where

import Control.Applicative

lowerA2 :: (Applicative f) => (f a -> f b -> f c) -> a -> b -> f c
lowerA2 f x y = f (pure x) (pure y)

