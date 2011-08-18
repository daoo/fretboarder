module Fretboarder.Parser.String where

import Fretboarder.Guitar.Note

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
