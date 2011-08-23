--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Tests where

import Test.QuickCheck

import Fretboarder.Parser.String

import Fretboarder.Guitar.INote
import Fretboarder.Guitar.Note

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

