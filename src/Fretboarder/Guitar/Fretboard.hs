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
mkFretboard = Fretboard . map noteToI

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = mkFretboard
  [ Note 4 E Natural
  , Note 3 B Natural
  , Note 3 G Natural
  , Note 3 D Natural
  , Note 2 A Natural
  , Note 2 E Natural
  ]
