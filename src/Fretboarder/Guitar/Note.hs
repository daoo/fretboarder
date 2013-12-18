{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Guitar.Note
  ( Octave
  , Tone(..)
  , Accidental(..)
  , Note(Note)

  , iToNote
  , noteToI
  , fixNote
  ) where

import Control.Applicative
import Fretboarder.Guitar.INote
import Test.QuickCheck

-- |An musical octave containing 12 notes.
newtype Octave = Octave { mkOctave :: Int }
  deriving (Eq, Enum, Ord, Num, Real, Integral)

instance Show Octave where
  show = show . mkOctave

instance Arbitrary Octave where
  arbitrary = Octave <$> choose (0, 10)

-- |A musical tone.
data Tone = A | B | C | D | E | F | G
  deriving (Show)

instance Arbitrary Tone where
  arbitrary = elements [A, B, C, D, E, F, G]

-- |A musical accidental.
data Accidental = Natural | Flat | Sharp
  deriving Show

instance Arbitrary Accidental where
  arbitrary = elements [Flat, Natural, Sharp]

-- |Representation for the scientific pitch notation.
--
-- This type is highly redundant and confusing to use. The INote type is much
-- better to use in code and Note should only be used for user interaction.
data Note = Note Octave Tone Accidental

instance Eq Note where
  a == b = noteToI a == noteToI b

instance Show Note where
  show (Note o t a) = case a of
    Flat    -> shows t $ shows o "b"
    Natural -> shows t $ show o
    Sharp   -> shows t $ shows o "#"

instance Arbitrary Note where
  arbitrary = Note <$> arbitrary <*> arbitrary <*> arbitrary

noteToI :: Note -> INote
noteToI (Note o t a) = fromIntegral $ (o' * 12) + t' + a'
  where
    o' = mkOctave o

    t' = case t of
      A -> 0
      B -> 2
      C -> 3
      D -> 5
      E -> 7
      F -> 8
      G -> 10

    a' = case a of
      Flat    -> -1
      Natural -> 0
      Sharp   -> 1

iToNote :: INote -> Note
iToNote i = Note (fromIntegral q) tone accidental
  where
    (q, r) = i `quotRem` 12

    (tone, accidental) = case r of
      0  -> (A, Natural)
      1  -> (A, Sharp)
      2  -> (B, Natural)
      3  -> (C, Natural)
      4  -> (C, Sharp)
      5  -> (D, Natural)
      6  -> (D, Sharp)
      7  -> (E, Natural)
      8  -> (F, Natural)
      9  -> (F, Sharp)
      10 -> (G, Natural)
      11 -> (G, Sharp)
      _  -> (A, Natural) -- this case can not happen because of rem 12

-- |Fix note representation for display.
--
-- For some reason beyond human understanding, we have that B# == C, B == Cb,
-- E# == F and E == Fb. The sharps and flats of these notes are not used at all
-- in written musical notation. This function normalizes those sharps and flats
-- to naturals.
fixNote :: Note -> Note
fixNote = \case
  Note o B Sharp -> Note (o+1) C Natural
  Note o C Flat  -> Note (o+1) B Natural
  Note o E Sharp -> Note o F Natural
  Note o F Flat  -> Note o E Natural
  n              -> n
