--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Extensions.Tuple where

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)
