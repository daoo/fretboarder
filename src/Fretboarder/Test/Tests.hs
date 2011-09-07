--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Tests where

import Data.List

import Test.QuickCheck

import Fretboarder.Parser.String

import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Interval
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale

instance Arbitrary Note where
  arbitrary = do
    t <- oneof $ map return [A, B, C, D, E, F, G]
    o <- oneof $ map return [-1, 0, 1, 2, 3, 4, 5]
    a <- oneof $ map return [Flat, Natural, Sharp]
    return $ Note t o a

instance Arbitrary Scale where
  arbitrary = do
    scale <- oneof $ map return [ majorScale, minorScale, harmonicMinor
                                , melodicMinor, minorPentatonic, majorPentatonic
                                , bluesScale, ionianMode, dorianMode
                                , phrygianMode, lydianMode, mixolydianMode
                                , aeolianMode, locrianMode ]
    note <- arbitrary
    return $ Scale note scale

hasDups :: (Eq a) => [a] -> Bool
hasDups []     = False
hasDups (x:xs) = x `elem` xs || hasDups xs

propHasDups :: (Eq a) => [a] -> Bool
propHasDups xs = nubTest xs == hasDups xs
  where
    nubTest [] = False
    nubTest xs = nub xs /= xs

propFromToINote :: INote -> Bool
propFromToINote i = i == (toINote $ fromINote i)

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

