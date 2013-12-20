{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.Semitone
  ( Semitone(fromSemitone)
  , mkSemitone

  , Offset
  , mkOffset
  , (+.)
  , off

  , ScaleOffset(fromScaleOffset)
  , mkScaleOffset
  , scaled
  ) where

import Control.Applicative
import Test.QuickCheck

-- |Represents a musical note.
--
-- Efficient non-reduntant representation. The value 0 is C0 and then it
-- increments in semi tones.
newtype Semitone = Semitone { fromSemitone :: Int }
  deriving (Eq, Ord, Enum, Num, Show)

mkSemitone :: Int -> Semitone
mkSemitone = Semitone

instance Arbitrary Semitone where
  arbitrary = Semitone <$> choose (-50, 200)

  shrink = map Semitone . shrink . fromSemitone

-- |Type for offset between two notes.
newtype Offset = Offset Int
  deriving (Eq, Ord, Enum, Show)

mkOffset :: Int -> Offset
mkOffset = Offset

instance Arbitrary Offset where
  arbitrary = Offset `fmap` choose (-100, 100)

(+.) :: Semitone -> Offset -> Semitone
Semitone a +. Offset b = Semitone (a + b)

off :: Semitone -> Semitone -> Offset
off a b = Offset (fromSemitone a - fromSemitone b)

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
