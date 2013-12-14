module Fretboarder.Guitar.Scale
  ( Scale(..)

  , majorOffsets
  , minorOffsets
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

  , hasNote
  , repeatScale
  ) where

import Control.Applicative
import Data.List
import Data.Monoid
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Offset
import Test.QuickCheck

-- Some predefined scale offsets
majorOffsets, minorOffsets :: [Offset]
majorOffsets = [0, 2, 4, 5, 7, 9, 11]
minorOffsets = [0, 2, 3, 5, 7, 8, 10]

harmonicMinor, melodicMinor :: [Offset]
harmonicMinor = [0, 2, 3, 5, 7, 8, 11]
melodicMinor  = [0, 2, 3, 5, 7, 9, 11]

minorPentatonic, majorPentatonic :: [Offset]
minorPentatonic = [0, 3, 5, 7, 10]
majorPentatonic = [0, 2, 4, 7, 9]

bluesOffsets :: [Offset]
bluesOffsets = [0, 6]

ionianMode, dorianMode, phrygianMode, lydianMode, mixolydianMode,
  aeolianMode, locrianMode :: [Offset]
ionianMode     = [0, 2, 4, 5, 7, 9, 11] -- Identical to the major scale
dorianMode     = [0, 2, 3, 5, 7, 9, 10] -- Minor scale with raised 6th
phrygianMode   = [0, 1, 3, 5, 7, 8, 10] -- Minor scale with lowered 2nd
lydianMode     = [0, 2, 4, 6, 7, 9, 11] -- Major scale with raised 4th
mixolydianMode = [0, 2, 4, 5, 7, 9, 10] -- Major scale with lowered 7th
aeolianMode    = [0, 2, 3, 5, 7, 8, 10] -- Identical to the minor scale
locrianMode    = [0, 1, 3, 5, 6, 8, 10] -- Minor scale with lowered 2nd and 5th

data Scale = Scale INote [Offset]
  deriving Show

instance Arbitrary Scale where
  arbitrary = Scale <$> arbitrary <*> elements scales
    where
      scales =
        [ majorOffsets
        , minorOffsets
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
        ]

hasNote :: Scale -> INote -> Bool
hasNote (Scale base offsets) note = elem (fromIntegral $ note - base) offsets

repeatScale :: Scale -> [INote]
repeatScale (Scale note offsets) = go note offsets
  where
    go x []     = go x offsets
    go x (y:ys) = x' : go x' ys
      where x' = addOffset x y

instance Monoid Scale where
  mempty = Scale 0 []

  mappend (Scale x xs) (Scale y ys) =
    Scale x $ sort $ union xs (map (+d) ys)
    where
      d = fromIntegral $ x-y
