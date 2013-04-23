module Fretboarder.Guitar.Interval where

-- | Describing Scales
-- Okay, we have three ways to describe the intervals between notes in a scale:
--   * Intervals,
--     Simple integer intervals where 1 equals to one semitone and 2 equals to
--     a note, 3 is a tone and a semitone, and so on. Can also be zero or
--     negative. Each integer in the list is relative to the integer just
--     before.
--   * Diatonic Intervals,
--     The same as integer intervals but each interval has a name:
--       * Major third = 2 tones
--       * Perfect fifth = 3 tones and a semitone
--       * etc...
--   * Offsets,
--     Again integers but is instead relative to the first note of the scale.
--
-- Internally I prefer to use offsets.

type Interval = Int
type Offset   = Int

data DiatonicInterval = PerfectUnison | Minor2 | Major2 | Minor3 | Major3
                      | Perfect4 | Tritone | Perfect5 | Minor6 | Major6
                      | Minor7 | Major7 | PerfectOctave
  deriving (Show, Eq, Enum)

-- Some predefined scale offsets
minorScale, majorScale :: [Offset]
majorScale = [2, 4, 5, 7, 9, 11, 12]
minorScale = [2, 3, 5, 7, 8, 10, 12]

harmonicMinor, melodicMinor :: [Offset]
harmonicMinor = [2, 3, 5, 7, 8, 11, 12]
melodicMinor  = [2, 3, 5, 7, 9, 11, 12]

minorPentatonic, majorPentatonic :: [Offset]
minorPentatonic = [3, 5, 7, 10, 12]
majorPentatonic = [2, 4, 7, 9, 12]

bluesScale :: [Offset]
bluesScale = [6]

ionianMode, dorianMode, phrygianMode, lydianMode, mixolydianMode, aeolianMode, locrianMode :: [Interval]
ionianMode     = [2, 4, 5, 7, 9, 11, 12] -- Identical to the major scale
dorianMode     = [2, 3, 5, 7, 9, 10, 12] -- Minor scale with raised 6th
phrygianMode   = [1, 3, 5, 7, 8, 10, 12] -- Minor scale with lowered 2nd
lydianMode     = [2, 4, 6, 7, 9, 11, 12] -- Major scale with raised 4th
mixolydianMode = [2, 4, 5, 7, 9, 10, 12] -- Major scale with lowered 7th
aeolianMode    = [2, 3, 5, 7, 8, 10, 12] -- Identical to the minor scale
locrianMode    = [1, 3, 5, 6, 8, 10, 12] -- Minor scale with lowered 2nd and 5th

diatonicToOffset :: DiatonicInterval -> Offset
diatonicToOffset = fromEnum

diatonicToIntervals :: Int -> [DiatonicInterval] -> [Interval]
diatonicToIntervals _ []     = []
diatonicToIntervals i (a:as) = b : diatonicToIntervals (i + b) as
  where
    b = d - i
    d = diatonicToOffset a
