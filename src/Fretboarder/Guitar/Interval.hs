{-# LANGUAGE LambdaCase #-}
module Fretboarder.Guitar.Interval
  ( Interval
  , Offset
  , DiatonicInterval(..)

  , minorScale
  , majorScale
  , harmonicMinor
  , melodicMinor
  , minorPentatonic
  , majorPentatonic
  , bluesScale
  , ionianMode
  , dorianMode
  , phrygianMode
  , lydianMode
  , mixolydianMode
  , aeolianMode
  , locrianMode

  , diatonicToOffset
  , diatonicToIntervals
  ) where

-- |Semitone intervals
-- 1 equals to one semitone and 2 equals to a tone, 3 is a tone plus a
-- semitone, and so on. Can also be zero or negative. In a list each interval
-- should be treated as relative to the preceding interval in the list.
type Interval = Int

-- |Semitone offset relative to a root note
-- Similar to Intervals where 1 is a semitone and 2 is a whole tone and so on.
-- But in a list the each offset is should be treated as relative to some root
-- note.
type Offset = Int

-- |Diatonic Intervals,
-- The same as integer intervals but each interval has a name:
--   * Major third = 2 tones
--   * Perfect fifth = 3 tones and a semitone
--   * etc...
data DiatonicInterval = PerfectUnison | Minor2 | Major2 | Minor3 | Major3
                      | Perfect4 | Tritone | Perfect5 | Minor6 | Major6
                      | Minor7 | Major7 | PerfectOctave
  deriving (Show, Eq)

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

ionianMode, dorianMode, phrygianMode, lydianMode, mixolydianMode,
  aeolianMode, locrianMode :: [Offset]
ionianMode     = [2, 4, 5, 7, 9, 11, 12] -- Identical to the major scale
dorianMode     = [2, 3, 5, 7, 9, 10, 12] -- Minor scale with raised 6th
phrygianMode   = [1, 3, 5, 7, 8, 10, 12] -- Minor scale with lowered 2nd
lydianMode     = [2, 4, 6, 7, 9, 11, 12] -- Major scale with raised 4th
mixolydianMode = [2, 4, 5, 7, 9, 10, 12] -- Major scale with lowered 7th
aeolianMode    = [2, 3, 5, 7, 8, 10, 12] -- Identical to the minor scale
locrianMode    = [1, 3, 5, 6, 8, 10, 12] -- Minor scale with lowered 2nd and 5th

diatonicToOffset :: DiatonicInterval -> Offset
diatonicToOffset = \case
  PerfectUnison -> 0
  Minor2        -> 1
  Major2        -> 2
  Minor3        -> 3
  Major3        -> 4
  Perfect4      -> 5
  Tritone       -> 6
  Perfect5      -> 7
  Minor6        -> 8
  Major6        -> 9
  Minor7        -> 10
  Major7        -> 11
  PerfectOctave -> 12

diatonicToIntervals :: Int -> [DiatonicInterval] -> [Offset]
diatonicToIntervals _ []     = []
diatonicToIntervals i (a:as) = b : diatonicToIntervals (i + b) as
  where
    b = d - i
    d = diatonicToOffset a
