{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.SPN
  ( Octave
  , Tone(..)
  , Accidental(..)
  , PitchClass(..)
  , SPN(SPN)

  , toSemi
  , fromSemi
  , fixSPN
  ) where

import Control.Applicative
import Fretboarder.Music.Semitone
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
  deriving (Eq, Show)

instance Arbitrary Tone where
  arbitrary = elements [A, B, C, D, E, F, G]

-- |A musical accidental.
data Accidental = Natural | Flat | Sharp
  deriving (Eq, Show)

instance Arbitrary Accidental where
  arbitrary = elements [Flat, Natural, Sharp]

data PitchClass = Cn | Cs | Dn | Ds | En | Fn | Fs | Gn | Gs | An | As | Bn

-- |Representation for the scientific pitch notation.
--
-- This type is highly redundant and confusing to use. The Semitone type is much
-- better to use in code and SPN should only be used for user interaction.
data SPN = SPN Octave Tone Accidental

instance Eq SPN where
  a == b = toSemi a == toSemi b

instance Show SPN where
  show (SPN o t a) = case a of
    Flat    -> shows t $ shows o "b"
    Natural -> shows t $ show o
    Sharp   -> shows t $ shows o "#"

instance Arbitrary SPN where
  arbitrary = SPN <$> arbitrary <*> arbitrary <*> arbitrary

toSemi :: SPN -> Semitone
toSemi (SPN o t a) = fromIntegral $ (o' * 12) + t' + a'
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

fromSemi :: Semitone -> SPN
fromSemi n = SPN (fromIntegral q) tone accidental
  where
    (q, r) = n `quotRem` 12

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
fixSPN :: SPN -> SPN
fixSPN = \case
  SPN o B Sharp -> SPN (o+1) C Natural
  SPN o C Flat  -> SPN (o+1) B Natural
  SPN o E Sharp -> SPN o F Natural
  SPN o F Flat  -> SPN o E Natural
  n             -> n
