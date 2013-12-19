module Fretboarder.Music.Fretboard
  ( Fretboard(tuning)
  , stringCount
  , ebgdae
  ) where

import Fretboarder.Music.Note
import Fretboarder.Music.SPN
import Fretboarder.Music.Semitone

-- |Represent a fretboard with a specific tuning
newtype Fretboard = Fretboard
  { tuning :: [Semitone] -- ^The notes for each string
  } deriving Show

stringCount :: Fretboard -> Int
stringCount = length . tuning

mkFretboard :: [SPN] -> Fretboard
mkFretboard = Fretboard . map toSemi

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = mkFretboard
  [ SPN 4 E Natural
  , SPN 3 B Natural
  , SPN 3 G Natural
  , SPN 3 D Natural
  , SPN 2 A Natural
  , SPN 2 E Natural
  ]
