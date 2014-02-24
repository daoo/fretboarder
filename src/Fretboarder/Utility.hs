{-# LANGUAGE BangPatterns #-}
module Fretboarder.Utility where

import Control.Arrow

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f = f *** f

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs
