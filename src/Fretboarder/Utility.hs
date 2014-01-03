{-# LANGUAGE BangPatterns #-}
module Fretboarder.Utility where

import Control.Arrow

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f = f *** f

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs

enumFilter :: (Ord a, Enum a) => a -> a -> (a -> Bool) -> (a -> b) -> [b]
enumFilter a b f g = go a
  where
    go !i | i > b     = []
          | f i       = g i : go (succ i)
          | otherwise = go (succ i)
