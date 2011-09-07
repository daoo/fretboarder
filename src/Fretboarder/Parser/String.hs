--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Parser.String where

import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Interval

instance Show Accidental where
  show Flat    = "b"
  show Natural = ""
  show Sharp   = "#"

instance Show Note where
  show (Note t o a) = concat [show t, show o, show a]

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

readIntervals :: String -> [Interval]
readIntervals "dorian" = dorianMode
