module Fretboarder.Guitar.Offset
  ( Offset
  , addOffset
  ) where

import Fretboarder.Guitar.Note

newtype Offset = Offset { mkOffset :: Int }
  deriving (Eq, Ord)

instance Show Offset where
  show = show . mkOffset

math :: (Int -> Int -> Int) -> Offset -> Offset -> Offset
math op a b = Offset (op (mkOffset a) (mkOffset b) `mod` 12)

instance Num Offset where
  (+) = math (+)
  (-) = math (-)
  (*) = math (*)

  negate (Offset x) = Offset (11 - x)

  signum = const (Offset 1)
  abs    = id

  fromInteger = Offset . (`mod` 12) . fromInteger

addOffset :: INote -> Offset -> INote
addOffset a (Offset b) = a + b
