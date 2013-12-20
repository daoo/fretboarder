module Fretboarder.Music.Scale
  ( Scale
  , hasOffset
  , fromOffsets
  , toOffsets
  , lower
  , raise
  ) where

import Data.Bits
import Data.List
import Data.Word
import Fretboarder.Music.Semitone
import Test.QuickCheck

-- |Type for a octave repeating scale.
newtype Scale = Scale { bitField :: Word16 }
  deriving (Eq, Show)

instance Arbitrary Scale where
  arbitrary = Scale `fmap` choose (1, 2^12-1)

hasOffset :: Scale -> ScaleOffset -> Bool
hasOffset s = testBit (bitField s) . fromScaleOffset

fromOffsets :: [ScaleOffset] -> Scale
fromOffsets = Scale . foldl' (\acc -> setBit acc . fromScaleOffset) 0

toOffsets :: Scale -> [ScaleOffset]
toOffsets (Scale bits) = map mkScaleOffset $ filter (testBit bits) [0..11]

moveBit :: Bits a => Int -> Int -> a -> a
moveBit d i a = setBit (clearBit a i) (i + d)

lower, raise :: Int -> Scale -> Scale
lower i (Scale bits) = Scale $ moveBit (-1) i bits
raise i (Scale bits) = Scale $ moveBit 1 i bits
