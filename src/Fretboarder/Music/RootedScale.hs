module Fretboarder.Music.RootedScale
  ( RootedScale(..)
  , hasNote
  ) where

import Fretboarder.Music.Scale
import Fretboarder.Music.Semitone

data RootedScale = RootedScale
  { root :: Semitone
  , scale :: Scale
  } deriving Show

hasNote :: Semitone -> RootedScale -> Bool
hasNote n (RootedScale r s) = hasOffset s (scaled $ n `off` r)
