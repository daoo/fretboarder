{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.SPN
  ( Octave
  , Tone(..)
  , Accidental(..)
  , PitchClass(..)
  , mkPitchClass
  , SPN(..)
  , mkSPN

  , tone
  , accidental

  , toOffset
  , fromOffset
  , toNote
  , fromNote
  ) where

import Control.Applicative
import Fretboarder.Music.Note
import Test.QuickCheck

-- |An musical octave containing 12 notes.
newtype Octave = Octave { mkOctave :: Int }
  deriving (Eq, Enum, Ord, Num, Real, Integral)

instance Show Octave where
  show = show . mkOctave

instance Arbitrary Octave where
  arbitrary = Octave <$> choose (-1, 11)

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
  deriving (Eq, Enum, Show)

mkPitchClass :: Tone -> Accidental -> PitchClass
mkPitchClass t a = case (t, a) of
  (C, Flat)    -> Bn
  (C, Natural) -> Cn
  (C, Sharp)   -> Cs
  (D, Flat)    -> Cs
  (D, Natural) -> Dn
  (D, Sharp)   -> Ds
  (E, Flat)    -> Ds
  (E, Natural) -> En
  (E, Sharp)   -> Fn
  (F, Flat)    -> En
  (F, Natural) -> Fn
  (F, Sharp)   -> Fs
  (G, Flat)    -> Fs
  (G, Natural) -> Gn
  (G, Sharp)   -> Gs
  (A, Flat)    -> Gs
  (A, Natural) -> An
  (A, Sharp)   -> As
  (B, Flat)    -> As
  (B, Natural) -> Bn
  (B, Sharp)   -> Cn

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
-- The Note type is much better to use in code and SPN should only be used
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

toOffset :: PitchClass -> ScaleOffset
toOffset = unsafeScaleOffset . fromEnum

fromOffset :: ScaleOffset -> PitchClass
fromOffset = toEnum . fromScaleOffset

toNote :: SPN -> Note
toNote (SPN o p) = fromIntegral $ fromIntegral o * 12 + fromEnum p

fromNote :: Note -> SPN
fromNote n = SPN (Octave q) (toEnum r)
  where
    (q, r) = fromIntegral n `divMod` 12
