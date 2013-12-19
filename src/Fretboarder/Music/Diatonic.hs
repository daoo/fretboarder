module Fretboarder.Music.Diatonic
  ( Diatonic(..)
  , diatonicToOffset
  ) where

import Fretboarder.Music.Offset

-- |Diatonic Intervals.
data Diatonic
  = PerfectUnison
  | Minor2
  | Major2
  | Minor3
  | Major3
  | Perfect4
  | Tritone
  | Perfect5
  | Minor6
  | Major6
  | Minor7
  | Major7
  | PerfectOctave
  deriving (Show, Eq, Ord, Enum)

diatonicToOffset :: Diatonic -> Offset
diatonicToOffset = fromIntegral . fromEnum
