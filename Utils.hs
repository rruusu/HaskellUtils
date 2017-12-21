{-
  A growing collection of generic utility functions of very generic use.
-}

module Utils where

-- Functional if
if' True x _ = x
if' False _ y = y
