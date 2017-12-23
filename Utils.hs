{-
  A growing collection of generic utility functions of very generic use.
-}

module Utils where

-- Functional if: if' c a b = if c then a else b
if' True x _ = x
if' False _ y = y

-- Left-binding low-precedence function application:
-- map & replaceWithWhen (const z) (<0) $ zipWith (-) & getx pts & gety pts
infixl 1 &
(&) :: (a -> b) -> a -> b
(&) = ($)

-- Very nice for conditional processing
replaceWithWhen :: (a -> a) -> (a -> Bool) -> a -> a
replaceWithWhen f b x = if' (b x) (f x) x

-- Very nice for conditional processing
replaceWithIf :: (a -> a) -> Bool -> a -> a
replaceWithIf f b x = if' b (f x) x
