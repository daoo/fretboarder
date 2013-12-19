{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.Note
  ( Octave
  , Tone(..)
  , Accidental(..)
  , Note(Note)

  , iToNote
  , noteToI
  , fixNote
  ) where

import Control.Applicative
import Fretboarder.Music.INote
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
      A -> 9
      B -> 11
      C -> 0
      D -> 2
      E -> 4
      F -> 5
      G -> 7

    a' = case a of
      Flat    -> -1
      Natural -> 0
      Sharp   -> 1

iToNote :: INote -> Note
iToNote i = Note (fromIntegral q) tone accidental
  where
    (q, r) = i `quotRem` 12

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
