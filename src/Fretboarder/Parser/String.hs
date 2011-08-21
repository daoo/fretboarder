--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Parser.String where

import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale
import Fretboarder.Guitar.Intervals

readTone :: Char -> Tone
readTone 'A' = A
readTone 'B' = B
readTone 'C' = C
readTone 'D' = D
readTone 'E' = E
readTone 'F' = F
readTone 'G' = G
readTone _   = error "No such tone."

readAccidental :: Char -> Accidental
readAccidental '#' = Sharp
readAccidental 'b' = Flat
readAccidental _   = Natural

readIntervals :: String -> Intervals
readIntervals "dorian" = dorianMode
