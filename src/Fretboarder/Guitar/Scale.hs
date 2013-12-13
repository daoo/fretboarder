module Fretboarder.Guitar.Scale
  ( Scale(..)
  , hasNote
  , repeatScale
  ) where

import Control.Applicative
import Data.List
import Data.Monoid
import Fretboarder.Guitar.Interval
import Fretboarder.Guitar.Note
import Test.QuickCheck

data Scale = Scale INote [Diatonic]
  deriving (Show)

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
      where x' = x + fromEnum y

instance Monoid Scale where
  mempty = Scale 0 []

  mappend (Scale x xs) (Scale y ys) =
    Scale x $ sort $ union xs (map (+ (toEnum $ x-y)) ys)
