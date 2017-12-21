{-# LANGUAGE FlexibleInstances #-}

{-
  MonoidF is a type modifier for interpreting applicative functors
  of monoids as monoids that combine their contained values
  using (liftA2 mappend).

  MonoidZip is a type modifier for interpreting traversable applicative
  functors of monoids as monoids that combine their contained values
  using (zipWithT mappend).
-}

module MonoidF where

import Control.Applicative
import ZipT

newtype MonoidF a = MonoidF a
  deriving (Eq, Show)

getMonoidF (MonoidF x) = x

instance Functor MonoidF where
  fmap f (MonoidF x) = MonoidF (f x)

instance Applicative MonoidF where
  pure = MonoidF
  MonoidF f <*> MonoidF x = MonoidF (f x)

-- Monoid instance
instance (Applicative f, Monoid a) => Monoid (MonoidF (f a)) where
  mempty = MonoidF (pure mempty)
  mappend = liftA2 (liftA2 mappend)


newtype MonoidZip a = MonoidZip a
  deriving (Eq, Show)

getMonoidZip (MonoidZip x) = x

instance Functor MonoidZip where
  fmap f (MonoidZip x) = MonoidZip (f x)

instance Applicative MonoidZip where
  pure = MonoidZip
  MonoidZip f <*> MonoidZip x = MonoidZip (f x)

-- Monoid instance
instance (Applicative f, Traversable f, Monoid a) => Monoid (MonoidZip (f a)) where
  mempty = MonoidZip (pure mempty)
  mappend = liftA2 (zipWithT mappend)
