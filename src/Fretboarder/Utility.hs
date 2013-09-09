module Fretboarder.Utility where

import Control.Arrow

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f = f *** f

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs

headOrEmpty :: [a] -> [a]
headOrEmpty []    = []
headOrEmpty (x:_) = [x]
