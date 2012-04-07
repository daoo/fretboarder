--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Scale where

import Test.QuickCheck

import Fretboarder.Guitar.Interval
import Fretboarder.Guitar.Note

data Scale = Scale INote [Offset]
  deriving (Show)

instance Arbitrary Scale where
  arbitrary = do
    scale <- oneof $ map return [ majorScale, minorScale, harmonicMinor
                                , melodicMinor, minorPentatonic, majorPentatonic
                                , bluesScale, ionianMode, dorianMode
                                , phrygianMode, lydianMode, mixolydianMode
                                , aeolianMode, locrianMode ]
    note <- arbitrary
    return $ Scale note scale

hasNote :: Scale -> INote -> Bool
hasNote (Scale base offsets) note = note' `elem` (0 : offsets)
  where
    note' = (note - base) `mod` 12

repeatScale :: Scale -> [INote]
repeatScale (Scale note offsets) = note : concatMap f xs
  where
    f n = map (+n) offsets
    xs  = iterate (+12) note

