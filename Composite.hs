{-# LANGUAGE FlexibleContexts #-}

{-
  A generic type composition module by Reino Ruusu, December 2017, Espoo, Finland

  The simple type composition tag defined here can do many of the tasks that
  monad transformers are used for, but in a much more generic way, allowing
  useful compositions of Functors, Foldables, Traversables, Applicatives and
  Monads (with certain restrictions).
-}

module Composite where

import Control.Applicative
import Control.Monad
import Data.Foldable

-- This type tags a composition type as a parameterized type constructor
newtype Comp m n a = Comp (m (n a))

-- Undecorate a tagged composition type
getComp :: Comp m n a -> m (n a)
getComp (Comp x) = x

-- Lift a raw outer type to the composition type (inner has to be Applicative)
lift1 :: (Functor m, Applicative n) => m a -> Comp m n a
lift1 = Comp . (pure <$>)

-- Lift a raw inner type to the composition type (outer has to be Applicative)
lift2 :: (Applicative m) => n a -> Comp m n a
lift2 = Comp . pure

-- Foldable instance
instance (Foldable m, Foldable n) => Foldable (Comp m n) where
  -- foldMap :: Monoid k => (a -> k) -> Comp m n a -> k
  foldMap f (Comp x) = foldr (\a l -> foldMap f a `mappend` l) mempty x 

-- Functor instance
instance (Functor m, Functor n) => Functor (Comp m n) where
  -- fmap :: (a -> b) -> (Comp m n a) -> (Comp m n b)
  fmap f (Comp x) = Comp (fmap (fmap f) x)

-- Applicative instance
instance (Applicative m, Applicative n) => Applicative (Comp m n) where
  -- pure :: a -> Comp m n a
  pure = Comp . pure . pure
  -- (<*>) :: Comp m n (a -> b) -> Comp m n a -> Comp m n b
  Comp f <*> Comp x = Comp (liftA2 (<*>) f x)

-- Monad instance (only works if the inner type is Traversable, unlike IO, for example)
instance (Monad m, Monad n, Traversable n) => Monad (Comp m n) where
  -- return = pure
  -- (>>=) :: Comp m n a -> (a -> Comp m n b) -> Comp m n b
  Comp x >>= f = Comp $ x >>= fmap join . sequence . fmap (getComp . f)
