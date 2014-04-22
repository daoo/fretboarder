module Fretboarder.Fretboard
  ( Fretboard(tuning)
  , stringCount
  , ebgdae
  ) where

import Music.Theory.Note
import Music.Theory.SPN

-- |Represent a fretboard with a specific tuning
newtype Fretboard = Fretboard
  { tuning :: [Note] -- ^The notes for each string
  } deriving Show

stringCount :: Fretboard -> Int
stringCount = length . tuning

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = Fretboard
  [ toNote $ mkSPN 4 E Natural
  , toNote $ mkSPN 3 B Natural
  , toNote $ mkSPN 3 G Natural
  , toNote $ mkSPN 3 D Natural
  , toNote $ mkSPN 2 A Natural
  , toNote $ mkSPN 2 E Natural
  ]
