module Fretboarder.Music.RootedScale
  ( RootedScale(..)
  , hasNote
  ) where

import Fretboarder.Music.Scale
import Music.Theory.Note

data RootedScale = RootedScale
  { root :: Note
  , scale :: Scale
  } deriving Show

hasNote :: Note -> RootedScale -> Bool
hasNote n (RootedScale r s) = hasOffset s (scaled $ n `off` r)
