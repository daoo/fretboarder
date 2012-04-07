--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Extensions.List where

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs

