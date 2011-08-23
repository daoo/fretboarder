--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Note where

import Data.Char
import Data.Maybe

import Fretboarder.Guitar.INote

type Octave = Int

data Tone = A | B | C | D | E | F | G
  deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Accidental = Natural | Flat | Sharp
  deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- We use Scientific Pitch Notation to specify a Note.
-- C4 is middle C and the octave increase one step between B and C.
-- E.g: A3, A#3/Bb3, B3/C4b, B3#/C4, C4#/D4b, etc...
data Note = Note Tone Octave Accidental
  deriving (Eq)

a4 = Note A 4 Natural
b4 = Note B 4 Natural
c4 = Note C 4 Natural
a5 = Note A 5 Natural
b5 = Note B 5 Natural

toINote :: Note -> INote
toINote (Note t o a) = 12 * o' + t' + a'
  where
    t' = case t of
      A -> 0
      B -> 2
      C -> 3
      D -> 5
      E -> 7
      F -> 8
      G -> 10
    o' = fromIntegral $ fromEnum o
    a' = case a of
      Flat    -> -1
      Natural -> 0
      Sharp   -> 1

fromINote :: INote -> Note
fromINote i = Note t o a
  where
    o      = fromIntegral $ i `div` 12
    (t, a) = case i `mod` 12 of
      0 -> (A, Natural)
      1 -> (A, Sharp)
      2 -> (B, Natural)
      3 -> (C, Natural)
      4 -> (C, Sharp)
      5 -> (D, Natural)
      6 -> (D, Sharp)
      7 -> (E, Natural)
      8 -> (F, Natural)
      9 -> (F, Sharp)
      10 -> (G, Natural)
      11 -> (G, Sharp)

-- For some reason I can't understand, B# == C, B == Cb, E# == F and E == Fb
-- This method normalizes those sharps and flats to naturals
fixNote :: Note -> Note
fixNote (Note B o Sharp) = Note C (o + 1) Natural
fixNote (Note C o Flat)  = Note B (o - 1) Natural
fixNote (Note E o Sharp) = Note F o Natural
fixNote (Note F o Flat)  = Note E o Natural
fixNote n                = n
