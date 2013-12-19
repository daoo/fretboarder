module Fretboarder.Music.Scale
  ( Scale(..)
  , raise
  , lower

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
import Fretboarder.Music.INote
import Fretboarder.Music.Offset
import Test.QuickCheck

-- Some predefined scale offsets
majorOffsets, minorOffsets :: [Offset]
majorOffsets = [0, 2, 4, 5, 7, 9, 11]
minorOffsets = [0, 2, 3, 5, 7, 8, 10]

harmonicMinor, melodicMinor :: [Offset]
harmonicMinor = raise 7 minorOffsets
melodicMinor  = lower 3 majorOffsets

minorPentatonic, majorPentatonic :: [Offset]
minorPentatonic = [0, 3, 5, 7, 10]
majorPentatonic = [0, 2, 4, 7, 9]

bluesOffsets :: [Offset]
bluesOffsets = [0, 6]

ionianMode, dorianMode, phrygianMode, lydianMode, mixolydianMode,
  aeolianMode, locrianMode :: [Offset]
ionianMode     = majorOffsets
dorianMode     = raise 6 minorOffsets
phrygianMode   = lower 2 minorOffsets
lydianMode     = raise 4 majorOffsets
mixolydianMode = lower 7 majorOffsets
aeolianMode    = minorOffsets
locrianMode    = lower 2 $ lower 5 $ minorOffsets

-- |Octave repeating musical scale.
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

hasNote :: INote -> Scale -> Bool
hasNote note (Scale root offsets) = fromIntegral (note - root) `elem` offsets

repeatScale :: Scale -> [INote]
repeatScale (Scale root offsets) = go root offsets
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
