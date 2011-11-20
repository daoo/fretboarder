--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Parser.String where

import Data.List

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

levenshtein :: String -> String -> Int
levenshtein s t = d !! length s !! length t 
  where 
    d = [[distance m n|n<-[0..length t]]|m<-[0..length s]]

    distance i 0 = i
    distance 0 j = j
    distance i j = minimum [ d!!(i-1)!!j+1
                           , d!!i!!(j-1)+1
                           , d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 1)]

readOffsets :: String -> [Offset]
readOffsets str = snd $ head lst
  where
    lst = sort $ map f offsets

    f :: (String, [Offset]) -> (Int, [Offset])
    f (scale, offsets) = (levenshtein scale str, offsets)

    offsets :: [(String, [Offset])]
    offsets = [ ("major", majorScale)
              , ("minor", minorScale)

              , ("harmonic minor", harmonicMinor)
              , ("melodic minor", melodicMinor)

              , ("blues", bluesScale)

              , ("ionian", ionianMode)
              , ("dorian", dorianMode)
              , ("phrygian", phrygianMode)
              , ("lydian", lydianMode)
              , ("mixolydian", mixolydianMode)
              , ("aeolian", aeolianMode)
              , ("locrian", locrianMode) ]
