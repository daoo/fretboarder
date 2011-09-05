--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Scale where

import Fretboarder.Guitar.Note

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

diatonicToIntervals :: Integer -> [DiatonicInterval] -> Intervals
diatonicToIntervals _ []     = []
diatonicToIntervals i (a:as) = b : diatonicToIntervals (i + b) as
  where
    b = d - i
    d = diatonicToSemitones a

-- Note that all scales also should include the "starting note".
-- This is handled by the intervalsToINotes function

minorScale, majorScale :: Intervals
majorScale = [2, 2, 1, 2, 2, 2, 1]
minorScale = [2, 1, 2, 2, 1, 2, 2]

harmonicMinor, melodicMinor :: Intervals
harmonicMinor = [2, 1, 2, 2, 1, 3, 1]
melodicMinor  = [2, 1, 2, 2, 2, 2, 1]

minorPentatonic, majorPentatonic :: Intervals
minorPentatonic = diatonicToIntervals 0 [Minor3, Perfect4, Perfect5, Minor7, PerfectOctave]
majorPentatonic = diatonicToIntervals 0 [Major2, Major3, Perfect5, Major6, PerfectOctave]

bluesScale :: Intervals
bluesScale = [3, 2, 1, 1, 3, 2]

ionianMode, dorianMode, phrygianMode, lydianMode, mixolydianMode, aeolianMode, locrianMode :: Intervals
ionianMode     = [2, 2, 1, 2, 2, 2, 1] -- Identical to the major scale
dorianMode     = [2, 1, 2, 2, 2, 1, 2] -- Minor scale with raised 6th
phrygianMode   = [1, 2, 2, 2, 1, 2, 2] -- Minor scale with lowered 2nd
lydianMode     = [2, 2, 2, 1, 2, 2, 1] -- Major scale with raised 4th
mixolydianMode = [2, 2, 1, 2, 2, 1, 2] -- Major scale with lowered 7th
aeolianMode    = [2, 1, 2, 2, 1, 2, 2] -- Identical to the minor scale
locrianMode    = [1, 2, 2, 1, 2, 2, 2] -- Minor scale with lowered 2nd and 5th

intervalsToINotes :: INote -> Intervals -> [INote]
intervalsToINotes = scanl (+)

