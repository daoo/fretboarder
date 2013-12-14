module Fretboarder.Utility where

import Control.Arrow

timesM :: Monad m => Int -> (Int -> m ()) -> m ()
timesM 0 _ = return ()
timesM i m = (m i) >> timesM (i-1) m

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f = f *** f

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs
