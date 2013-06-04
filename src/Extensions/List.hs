module Extensions.List where

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs

headOrEmpty :: [a] -> [a]
headOrEmpty []    = []
headOrEmpty (x:_) = [x]
