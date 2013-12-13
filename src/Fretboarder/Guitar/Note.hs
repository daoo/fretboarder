{-# LANGUAGE LambdaCase #-}
module Fretboarder.Guitar.Note
  ( INote
  , Octave
  , Tone(..)
  , Accidental(..)
  , Note(..)

  , a4
  , b4
  , c4
  , a5
  , b5

  , toINote
  , fromINote
  , fixNote
  ) where

import Control.Applicative
import Test.QuickCheck

-- An internal note represntation where natural A in octave 0 represents the INote
-- number 0. A# in octave 0: INote 1, and so forth.
-- The same as the keys on a piano, if you start counting at 0 instead of 1.
type INote = Int

type Octave = Int

data Tone = A | B | C | D | E | F | G
  deriving (Show)

instance Arbitrary Tone where
  arbitrary = elements [A, B, C, D, E, F, G]

data Accidental = Natural | Flat | Sharp
  deriving Show

instance Arbitrary Accidental where
  arbitrary = elements [Flat, Natural, Sharp]

-- We use Scientific Pitch Notation to specify a Note.
-- C4 is middle C and the octave increase one step between B and C.
-- E.g: A3, A3#/B3b, B3/C4b, B3#/C4, C4#/D4b, etc...
data Note = Note Tone Octave Accidental

instance Show Note where
  show (Note t o a) = case a of
    Flat    -> shows t $ shows o "b"
    Natural -> shows t $ show o
    Sharp   -> shows t $ shows o "#"

instance Arbitrary Note where
  arbitrary = Note <$> arbitrary <*> choose (-1, 5) <*> arbitrary

a4, b4, c4, a5, b5 :: Note
a4 = Note A 4 Natural
b4 = Note B 4 Natural
c4 = Note C 4 Natural
a5 = Note A 5 Natural
b5 = Note B 5 Natural

toINote :: Note -> INote
toINote (Note t o a) = octave o + tone t + acc a
  where
    octave x = x * 12 - 9

    tone = \case
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7
      A -> 9
      B -> 11

    acc = \case
      Flat    -> -1
      Natural -> 0
      Sharp   -> 1

fromINote :: INote -> Note
fromINote i = Note tone q accidental
  where
    (q, r) = fromIntegral (i+9) `quotRem` 12

    (tone, accidental) = case r of
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
      _  -> (C, Natural) -- Default to C

-- For some reason I can't understand, B# == C, B == Cb, E# == F and E == Fb
-- This method normalizes those sharps and flats to naturals
fixNote :: Note -> Note
fixNote = \case
  Note B o Sharp -> Note C (o+1) Natural
  Note C o Flat  -> Note B (o-1) Natural
  Note E o Sharp -> Note F o Natural
  Note F o Flat  -> Note E o Natural
  n              -> n
