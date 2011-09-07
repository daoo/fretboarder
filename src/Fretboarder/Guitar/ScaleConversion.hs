--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.ScaleConversion where

import Fretboarder.Guitar.Scale

diatonicToOffset :: DiatonicInterval -> Offset
diatonicToOffset = toInteger . fromEnum

diatonicToIntervals :: Integer -> [DiatonicInterval] -> [Interval]
diatonicToIntervals _ []     = []
diatonicToIntervals i (a:as) = b : diatonicToIntervals (i + b) as
  where
    b = d - i
    d = diatonicToSemitones a

