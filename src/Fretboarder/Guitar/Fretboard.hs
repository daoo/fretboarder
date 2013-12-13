module Fretboarder.Guitar.Fretboard
  ( Fretboard(tuning)
  , stringCount
  , ebgdae
  ) where

import Fretboarder.Guitar.Note

-- |Represent a fretboard with a specific tuning
newtype Fretboard = Fretboard
  { tuning :: [INote] -- ^The notes for each string
  }

stringCount :: Fretboard -> Int
stringCount = length . tuning

mkFretboard :: [Note] -> Fretboard
mkFretboard = Fretboard . map toINote

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = mkFretboard
  [ Note E 4 Natural
  , Note B 3 Natural
  , Note G 3 Natural
  , Note D 3 Natural
  , Note A 2 Natural
  , Note E 2 Natural
  ]
