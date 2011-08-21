--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Scale where

import Fretboarder.Guitar.INote
import Fretboarder.Guitar.Intervals

type Scale = [INote]

minorScale, majorScale :: Intervals
majorScale = [0, 2, 2, 1, 2, 2, 2, 1]
minorScale = [0, 2, 1, 2, 2, 1, 2, 2]

harmonicMinor, melodicMinor :: Intervals
harmonicMinor = [0, 2, 1, 2, 2, 1, 3, 1]
melodicMinor  = [0, 2, 1, 2, 2, 2, 2, 1]

minorPentatonic, majorPentatonic :: Intervals
minorPentatonic = diatonicToIntervals 0 [PerfectUnison, Minor3, Perfect4, Perfect5, Minor7, PerfectOctave]
majorPentatonic = diatonicToIntervals 0 [PerfectUnison, Major2, Major3, Perfect5, Major6, PerfectOctave]

bluesScale :: Intervals
bluesScale = [0, 3, 2, 1, 1, 3, 2]

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
