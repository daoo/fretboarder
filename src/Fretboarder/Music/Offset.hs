{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.Offset
  ( Offset
  , addOffset
  , lower
  , raise
  ) where

import Fretboarder.Music.Semitone

-- |Type for offset within a pitch class.
--
-- Used for defining scales and chords. These offsets are relative to some root
-- note that is defined by context.
newtype Offset = Offset { mkOffset :: Int }
  deriving (Eq, Ord)

instance Show Offset where
  show = show . mkOffset

math :: (Int -> Int -> Int) -> Offset -> Offset -> Offset
math op (Offset a) (Offset b) = Offset (op a b `mod` 12)

instance Num Offset where
  (+) = math (+)
  (-) = math (-)
  (*) = math (*)

  negate (Offset x) = Offset (11 - x)

  signum = const (Offset 1)
  abs    = id

  fromInteger = Offset . (`mod` 12) . fromInteger

addOffset :: Semitone -> Offset -> Semitone
addOffset a (Offset b) = a + fromIntegral b

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt _ _ []     = []
mapAt 0 f (x:xs) = f x : xs
mapAt i f (x:xs) = x : mapAt (i-1) f xs

lower, raise :: Int -> [Offset] -> [Offset]
lower i = mapAt (i-1) (subtract 1)
raise i = mapAt (i+1) (subtract 1)
