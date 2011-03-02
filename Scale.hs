module Scale where

import Note
import Intervals

type Scale = [Note]

minorIntervals, majorIntervals, pentatonicIntervals :: Intervals
majorIntervals = [2, 2, 1, 2, 2, 2, 1]
minorIntervals = [2, 1, 2, 2, 1, 2, 2]
pentatonicIntervals = asdf 0 [Minor3, Perfect4, Perfect5, Minor7, PerfectOctave]

makeScale :: Note -> Intervals -> Scale
makeScale b intv = b : helper b intv
  where
    helper _ []     = []
    helper t (i:is) = n : helper n is
      where
        n = fixNote $ interval i t
