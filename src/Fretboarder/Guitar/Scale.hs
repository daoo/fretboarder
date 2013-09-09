module Fretboarder.Guitar.Scale
  ( Scale(..)
  , hasNote
  , repeatScale
  , joinScales
  ) where

import Control.Applicative
import Data.List
import Fretboarder.Guitar.Interval
import Fretboarder.Guitar.Note
import Test.QuickCheck

data Scale = Scale INote [Offset]
  deriving (Show)

instance Arbitrary Scale where
  arbitrary = Scale <$> arbitrary <*> oneof (map return scales)
    where
      scales = [ majorScale, minorScale, harmonicMinor, melodicMinor
               , minorPentatonic, majorPentatonic, bluesScale, ionianMode
               , dorianMode, phrygianMode, lydianMode, mixolydianMode
               , aeolianMode, locrianMode
               ]

hasNote :: Scale -> INote -> Bool
hasNote (Scale base offsets) note = note' `elem` (0 : offsets)
  where
    note' = (note - base) `mod` 12

repeatScale :: Scale -> [INote]
repeatScale (Scale note offsets) = note : concatMap f xs
  where
    f n = map (+n) offsets
    xs  = iterate (+12) note

joinScales :: Scale -> Scale -> Scale
joinScales (Scale n1 os1) (Scale n2 os2) = Scale n1 $ sort $ union os1 (map (+(n1 - n2)) os2)
