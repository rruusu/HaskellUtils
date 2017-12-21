{-
  Iteration is a monad for introducing a simple sequence of values
  into a functional calculation from any Foldable data type.

  Use next and hasMore to access the values, and runIteration or
  execIteration to fetch the result of the calculation.

  execIteration produces a tuple with the result of the calculation
  and a list of the remaining unaccesses items.

  Example:
  > execIteration ((+) <$> next <*> next) [1, 2, 3, 4]
  (3,[3,4])

  Warning: An exception is thrown if next is called too many times
-}

module Iterate where

import Data.Foldable

newtype Iteration i a = Iteration ([i] -> (a,[i]))

execIteration :: (Foldable f) => Iteration i a -> f i -> (a, [i])
execIteration (Iteration f) l = f (toList l)

runIteration :: (Foldable f) => Iteration i a -> f i -> a
runIteration (Iteration f) l = fst $ f (toList l)

next :: Iteration i i
next = Iteration (\l -> case l of (x:xs) -> (x, xs)
                                  [] -> error "next: input exhausted")

hasMore :: Iteration i Bool
hasMore = Iteration (\l -> (not (null l), l))

instance Functor (Iteration i) where
  fmap f (Iteration g) = Iteration (\l -> let (v, l2) = g l in (f v, l2))

instance Applicative (Iteration i) where
  pure = return
  f <*> x = f >>= (x >>=) . (return .)

instance Monad (Iteration i) where
  return x = Iteration (\l -> (x, l))
  f >>= g = Iteration (\l -> case execIteration f l of (v, l2) -> execIteration (g v) l2)
