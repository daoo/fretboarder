--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Note where

import Control.Applicative
import Test.QuickCheck

-- An internal note represntation where natural A in octave 0 represents the INote
-- number 0. A# in octave 0: INote 1, and so forth.
-- The same as the keys on a piano, if you start counting at 0 instead of 1.
type INote = Integer

type Octave = Integer

data Tone = A | B | C | D | E | F | G
  deriving (Show)

instance Arbitrary Tone where
  arbitrary = oneof $ map return [A, B, C, D, E, F, G]

data Accidental = Natural | Flat | Sharp

instance Show Accidental where
  show Flat    = "b"
  show Natural = ""
  show Sharp   = "#"

instance Arbitrary Accidental where
  arbitrary = oneof $ map return [Flat, Natural, Sharp]

-- We use Scientific Pitch Notation to specify a Note.
-- C4 is middle C and the octave increase one step between B and C.
-- E.g: A3, A3#/B3b, B3/C4b, B3#/C4, C4#/D4b, etc...
data Note = Note Tone Octave Accidental

instance Show Note where
  show (Note t o a) = shows t $ shows o $ show a

instance Arbitrary Note where
  arbitrary = liftA3 Note arbitrary (choose (-1, 5)) arbitrary

a4, b4, c4, a5, b5 :: Note
a4 = Note A 4 Natural
b4 = Note B 4 Natural
c4 = Note C 4 Natural
a5 = Note A 5 Natural
b5 = Note B 5 Natural

toINote :: Note -> INote
toINote (Note t o a) = o' + t' + a'
  where
    o' = o * 12 - 9
    t' = case t of
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7
      A -> 9
      B -> 11
    a' = case a of
      Flat    -> -1
      Natural -> 0
      Sharp   -> 1

fromINote :: INote -> Note
fromINote i = Note t o a
  where
    i' = i + 9
    o  = floor $ fromIntegral i' / 12.0

    (t, a) = case i' `mod` 12 of
      0  -> (C, Natural)
      1  -> (C, Sharp)
      2  -> (D, Natural)
      3  -> (D, Sharp)
      4  -> (E, Natural)
      5  -> (F, Natural)
      6  -> (F, Sharp)
      7  -> (G, Natural)
      8  -> (G, Sharp)
      9  -> (A, Natural)
      10 -> (A, Sharp)
      11 -> (B, Natural)
      _  -> error "Should not happen"

-- For some reason I can't understand, B# == C, B == Cb, E# == F and E == Fb
-- This method normalizes those sharps and flats to naturals
fixNote :: Note -> Note
fixNote n = case n of
  Note B o Sharp -> Note C (o + 1) Natural
  Note C o Flat  -> Note B (o - 1) Natural
  Note E o Sharp -> Note F o Natural
  Note F o Flat  -> Note E o Natural
  _              -> n
