module Fretboarder.Music.Fretboard
  ( Fretboard(tuning)
  , stringCount
  , ebgdae
  ) where

import Fretboarder.Music.SPN
import Fretboarder.Music.Note

-- |Represent a fretboard with a specific tuning
newtype Fretboard = Fretboard
  { tuning :: [Note] -- ^The notes for each string
  } deriving Show

stringCount :: Fretboard -> Int
stringCount = length . tuning

mkFretboard :: [SPN] -> Fretboard
mkFretboard = Fretboard . map toSemi

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = mkFretboard
  [ mkSPN 4 E Natural
  , mkSPN 3 B Natural
  , mkSPN 3 G Natural
  , mkSPN 3 D Natural
  , mkSPN 2 A Natural
  , mkSPN 2 E Natural
  ]
