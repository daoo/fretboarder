module Tests where

import Data.List
import Fretboarder.Guitar.INote
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale

propFromToINote :: INote -> Bool
propFromToINote i = i == noteToI (iToNote i)

equalINotes :: Bool
equalINotes = all (\ (a, b) -> noteToI a == noteToI b) notes
  where
    notes =
      [ (Note 4 A Sharp   , Note 4 B Flat)
      , (Note 4 B Natural , Note 5 C Flat)
      , (Note 4 B Sharp   , Note 5 C Natural)
      , (Note 4 E Natural , Note 4 F Flat)
      , (Note 4 E Sharp   , Note 4 F Natural)
      ]
