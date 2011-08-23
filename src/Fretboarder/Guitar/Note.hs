--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Note where

import Data.Char
import Data.Maybe

import Fretboarder.Guitar.INote

type Octave = Integer

data Tone = A | B | C | D | E | F | G
  deriving (Show)

data Accidental = Natural | Flat | Sharp

-- We use Scientific Pitch Notation to specify a Note.
-- C4 is middle C and the octave increase one step between B and C.
-- E.g: A3, A3#/B3b, B3/C4b, B3#/C4, C4#/D4b, etc...
data Note = Note Tone Octave Accidental

a4 = Note A 4 Natural
b4 = Note B 4 Natural
c4 = Note C 4 Natural
a5 = Note A 5 Natural
b5 = Note B 5 Natural

toINote :: Note -> INote
toINote (Note t o a) = o' + t' + a'
  where
    t' = case t of
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7
      A -> 9
      B -> 11
    o' = o * 12 - 9
    a' = case a of
      Flat    -> -1
      Natural -> 0
      Sharp   -> 1

fromINote :: INote -> Note
fromINote i = Note t o a
  where
    i'     = i + 9
    o      = truncate $ (realToFrac i') / 12.0
    (t, a) = case i' `mod` 12 of
      0 -> (C, Natural)
      1 -> (C, Sharp)
      2 -> (D, Natural)
      3 -> (D, Sharp)
      4 -> (E, Natural)
      5 -> (F, Natural)
      6 -> (F, Sharp)
      7 -> (G, Natural)
      8 -> (G, Sharp)
      9 -> (A, Natural)
      10 -> (A, Sharp)
      11 -> (B, Natural)

-- For some reason I can't understand, B# == C, B == Cb, E# == F and E == Fb
-- This method normalizes those sharps and flats to naturals
fixNote :: Note -> Note
fixNote (Note B o Sharp) = Note C (o + 1) Natural
fixNote (Note C o Flat)  = Note B (o - 1) Natural
fixNote (Note E o Sharp) = Note F o Natural
fixNote (Note F o Flat)  = Note E o Natural
fixNote n                = n
