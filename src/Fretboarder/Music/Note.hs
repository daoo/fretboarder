{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.Note
  ( Note(Note, fromNote)
  , scaleOffset

  , Offset(Offset)
  , (+.)
  , off

  , ScaleOffset(fromScaleOffset)
  , mkScaleOffset
  , scaled
  ) where

import Control.Applicative
import Test.QuickCheck

-- |Type for musical notes.
--
-- The value 0 is C0 and then it increments with semitone steps.
newtype Note = Note { fromNote :: Int }
  deriving (Eq, Ord, Enum, Num, Show)

-- |Calculate the scale offset relative to a scale rooted in C.
scaleOffset :: Note -> ScaleOffset
scaleOffset (Note n) = ScaleOffset (mod n 12)

instance Arbitrary Note where
  arbitrary = Note <$> choose (-50, 200)

  shrink = map Note . shrink . fromNote

-- |Type for the semitone distance between two notes.
newtype Offset = Offset Int
  deriving (Eq, Ord, Enum, Show)

instance Arbitrary Offset where
  arbitrary = Offset `fmap` choose (-100, 100)

(+.) :: Note -> Offset -> Note
Note a +. Offset b = Note (a + b)

off :: Note -> Note -> Offset
off a b = Offset (fromNote a - fromNote b)

-- |Type for the distance between two pitches within a pitch class.
newtype ScaleOffset = ScaleOffset { fromScaleOffset :: Int }
  deriving (Eq, Ord, Show)

instance Num ScaleOffset where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined

  fromInteger = ScaleOffset . fromInteger

mkScaleOffset :: Int -> ScaleOffset
mkScaleOffset i | i >= 0 && i <= 11 = ScaleOffset i
                | otherwise         = error "Scale offset out of range"

instance Arbitrary ScaleOffset where
  arbitrary = ScaleOffset `fmap` choose (0, 11)

scaled :: Offset -> ScaleOffset
scaled (Offset a) = ScaleOffset (mod a 12)
