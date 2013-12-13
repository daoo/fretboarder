{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Guitar.Interval
  ( Diatonic(..)

  , minorOffsets
  , majorOffsets
  , harmonicMinor
  , melodicMinor
  , minorPentatonic
  , majorPentatonic
  , bluesOffsets
  , ionianMode
  , dorianMode
  , phrygianMode
  , lydianMode
  , mixolydianMode
  , aeolianMode
  , locrianMode
  ) where

-- |Diatonic Intervals.
data Diatonic
  = PerfectUnison
  | Minor2
  | Major2
  | Minor3
  | Major3
  | Perfect4
  | Tritone
  | Perfect5
  | Minor6
  | Major6
  | Minor7
  | Major7
  | PerfectOctave
  deriving (Show, Eq, Ord, Enum)

math :: (Int -> Int -> Int) -> Diatonic -> Diatonic -> Diatonic
math op a b = toEnum $ op (fromEnum a) (fromEnum b) `mod` 13

instance Num Diatonic where
  (+) = math (+)
  (-) = math (-)
  (*) = math (*)

  negate a = toEnum $ 13 - fromEnum a

  abs    = id
  signum = const Minor2

  fromInteger = toEnum . fromInteger

-- Some predefined scale offsets
minorOffsets, majorOffsets :: [Diatonic]
majorOffsets = [0, 2, 4, 5, 7, 9, 11]
minorOffsets = [0, 2, 3, 5, 7, 8, 10]

harmonicMinor, melodicMinor :: [Diatonic]
harmonicMinor = [0, 2, 3, 5, 7, 8, 11]
melodicMinor  = [0, 2, 3, 5, 7, 9, 11]

minorPentatonic, majorPentatonic :: [Diatonic]
minorPentatonic = [0, 3, 5, 7, 10]
majorPentatonic = [0, 2, 4, 7, 9]

bluesOffsets :: [Diatonic]
bluesOffsets = [0, 6]

ionianMode, dorianMode, phrygianMode, lydianMode, mixolydianMode,
  aeolianMode, locrianMode :: [Diatonic]
ionianMode     = [0, 2, 4, 5, 7, 9, 11] -- Identical to the major scale
dorianMode     = [0, 2, 3, 5, 7, 9, 10] -- Minor scale with raised 6th
phrygianMode   = [0, 1, 3, 5, 7, 8, 10] -- Minor scale with lowered 2nd
lydianMode     = [0, 2, 4, 6, 7, 9, 11] -- Major scale with raised 4th
mixolydianMode = [0, 2, 4, 5, 7, 9, 10] -- Major scale with lowered 7th
aeolianMode    = [0, 2, 3, 5, 7, 8, 10] -- Identical to the minor scale
locrianMode    = [0, 1, 3, 5, 6, 8, 10] -- Minor scale with lowered 2nd and 5th
