module Fretboarder.Music.Diatonic
  ( Diatonic(..)
  ) where

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
