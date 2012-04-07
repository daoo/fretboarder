--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Tests where

import Data.List

import Fretboarder.Extensions.List
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale

propHasDups :: (Eq a) => [a] -> Bool
propHasDups xs = nubTest xs == hasDups xs
  where
    nubTest [] = False
    nubTest ys = nub ys /= ys

propFromToINote :: INote -> Bool
propFromToINote i = i == toINote (fromINote i)

propRepeat :: Scale -> Bool
propRepeat s = not . hasDups $ take 100 $ repeatScale s

propHasNotes :: Scale -> Bool
propHasNotes scale = all (hasNote scale) xs
  where
    xs = take 100 $ repeatScale scale

equalINotes :: Bool
equalINotes = all (\ (a, b) -> toINote a == toINote b) notes
  where
    notes =
      [ (Note A 4 Sharp, Note B 4 Flat)
      , (Note B 4 Natural, Note C 5 Flat)
      , (Note B 4 Sharp, Note C 5 Natural)
      , (Note E 4 Natural, Note F 4 Flat)
      , (Note E 4 Sharp, Note F 4 Natural)
      ]

