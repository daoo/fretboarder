module Fretboarder.Extensions.List where

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

