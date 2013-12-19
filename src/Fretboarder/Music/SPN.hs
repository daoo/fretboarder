{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.SPN
  ( Octave
  , Tone(..)
  , Accidental(..)
  , PitchClass(..)
  , SPN(..)
  , mkSPN

  , tone
  , accidental

  , toOffset
  , fromOffset
  , toSemi
  , fromSemi
  ) where

import Control.Applicative
import Fretboarder.Music.Offset
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
  deriving (Eq, Enum)

instance Arbitrary PitchClass where
  arbitrary = elements [Cn, Cs, Dn, Ds, En, Fn, Fs, Gn, Gs, An, As, Bn]

tone :: PitchClass -> Tone
tone = \case
  Cn -> C
  Cs -> C
  Dn -> D
  Ds -> D
  En -> E
  Fn -> F
  Fs -> F
  Gn -> G
  Gs -> G
  An -> A
  As -> A
  Bn -> B

accidental :: PitchClass -> Accidental
accidental = \case
  Cn -> Natural
  Cs -> Sharp
  Dn -> Natural
  Ds -> Sharp
  En -> Natural
  Fn -> Natural
  Fs -> Sharp
  Gn -> Natural
  Gs -> Sharp
  An -> Natural
  As -> Sharp
  Bn -> Natural

-- |Non-redundant type for the scientific pitch notation.
--
-- The Semitone type is much better to use in code and SPN should only be used
-- for user interaction.
data SPN = SPN
  { octave :: {-# UNPACK #-} !Octave
  , pitchClass :: !PitchClass
  }
  deriving Eq

mkSPN :: Octave -> Tone -> Accidental -> SPN
mkSPN o t a = case (t, a) of
  (C, Flat)    -> SPN (o-1) Bn
  (C, Natural) -> SPN o Cn
  (C, Sharp)   -> SPN o Cs
  (D, Flat)    -> SPN o Cs
  (D, Natural) -> SPN o Dn
  (D, Sharp)   -> SPN o Ds
  (E, Flat)    -> SPN o Ds
  (E, Natural) -> SPN o En
  (E, Sharp)   -> SPN o Fn
  (F, Flat)    -> SPN o En
  (F, Natural) -> SPN o Fn
  (F, Sharp)   -> SPN o Fs
  (G, Flat)    -> SPN o Fs
  (G, Natural) -> SPN o Gn
  (G, Sharp)   -> SPN o Gs
  (A, Flat)    -> SPN o Gs
  (A, Natural) -> SPN o An
  (A, Sharp)   -> SPN o As
  (B, Flat)    -> SPN o As
  (B, Natural) -> SPN o Bn
  (B, Sharp)   -> SPN (o+1) Cn

instance Show SPN where
  show (SPN o p) = case accidental p of
    Flat    -> shows (tone p) $ shows o "b"
    Natural -> shows (tone p) $ show o
    Sharp   -> shows (tone p) $ shows o "#"

instance Arbitrary SPN where
  arbitrary = SPN <$> arbitrary <*> arbitrary

toOffset :: PitchClass -> Offset
toOffset = toEnum . fromEnum

fromOffset :: Offset -> PitchClass
fromOffset = toEnum . fromEnum

toSemi :: SPN -> Semitone
toSemi (SPN o p) = fromIntegral $ (fromIntegral o * 12) + fromEnum p

fromSemi :: Semitone -> SPN
fromSemi n = SPN (fromIntegral q) (toEnum r)
  where
    (q, r) = fromIntegral n `quotRem` 12
