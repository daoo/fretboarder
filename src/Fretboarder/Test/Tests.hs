--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Tests where

import Data.List

import Test.QuickCheck

import Fretboarder.Parser.String

import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Note

elem2Test :: INote -> [INote] -> Bool
elem2Test i is = fst (elem2 i is') == elem i is'
  where is' = sort is

fromToINote :: INote -> Bool
fromToINote i = i == (toINote $ fromINote i)

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

