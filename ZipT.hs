{-
  This module contains zip and zipWith functions that are generalized for
  any Traversables. The first argument type only needs to be a
  Foldable.

  Warning: The first argument has to contain enough elements to
  match the length of the second argument, or an exception is thrown.
  This limitation is caused by the inherent restrictions of what
  can be done with a Traversable.
-}

module ZipT where

import Iterate
import Utils

zipWithT :: (Traversable t, Foldable f) => (a -> b -> c) -> f a -> t b -> t c
zipWithT f x y = flip runIteration x $ traverse mapElement y
  where mapElement y = if' <$> hasMore <*> (flip f y <$> next) <*> pure (error "zipWithT: Size mismatch")

zipT :: (Traversable t, Foldable f) => f a -> t b -> t (a,b)
zipT = zipWithT (,)
