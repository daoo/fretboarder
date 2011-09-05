--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Intervals where

-- 1 = Semitone
-- 2 = Tone
-- 3 = Tone + Semitone
type Interval  = Integer
type Intervals = [Interval]

data DiatonicInterval = PerfectUnison | Minor2 | Major2 | Minor3 | Major3 | Perfect4 | Tritone | Perfect5 |
                        Minor6 | Major6 | Minor7 | Major7 | PerfectOctave
  deriving (Show, Eq, Enum)

diatonicToSemitones :: DiatonicInterval -> Interval
diatonicToSemitones = toInteger . fromEnum

diatonicToIntervals _ []     = []
diatonicToIntervals i (a:as) = b : diatonicToIntervals (i + b) as
  where
    b = d - i
    d = diatonicToSemitones a

