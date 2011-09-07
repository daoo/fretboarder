--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Extensions.List where

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs

