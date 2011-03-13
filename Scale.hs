module Scale where

import INote
import Intervals

type Scale = [INote]

minorScale, majorScale :: Intervals
majorScale = [0, 2, 2, 1, 2, 2, 2, 1]
minorScale = [0, 2, 1, 2, 2, 1, 2, 2]

minorPentatonic, majorPentatonic :: Intervals
minorPentatonic = diatonicToIntervals 0 [PerfectUnison, Minor3, Perfect4, Perfect5, Minor7, PerfectOctave]
majorPentatonic = diatonicToIntervals 0 [PerfectUnison, Major2, Major3, Perfect5, Major6, PerfectOctave]

ionianMode, dorianMode, phrygianMode, lydianMode, mixolydianMode, aeolianMode, locrianMode :: Intervals
ionianMode     = [0, 2, 2, 1, 2, 2, 2, 1] -- Identical to the major scale
dorianMode     = [0, 2, 1, 2, 2, 2, 1, 2] -- Minor scale with raised 6th
phrygianMode   = [0, 1, 2, 2, 2, 1, 2, 2] -- Minor scale with lowered 2nd
lydianMode     = [0, 2, 2, 2, 1, 2, 2, 1] -- Major scale with raised 4th
mixolydianMode = [0, 2, 2, 1, 2, 2, 1, 2] -- Major scale with lowered 7th
aeolianMode    = [0, 2, 1, 2, 2, 1, 2, 2] -- Identical to the minor scale
locrianMode    = [0, 1, 2, 2, 1, 2, 2, 2] -- Minor scale with lowered 2nd and 5th

makeScale :: INote -> Intervals -> Scale
makeScale _ []     = []
makeScale t (i:is) = n : makeScale n is
  where
    n = t + i
