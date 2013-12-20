{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.Offset
  ( Offset
  , addOffset
  , lower
  , raise
  ) where

import Fretboarder.Music.Semitone
import Test.QuickCheck

-- |Type for offset within a pitch class.
--
-- Used for defining scales and chords. These offsets are relative to some root
-- note that is defined by context.
newtype Offset = Offset { mkOffset :: Int }
  deriving (Eq, Ord)

lift :: Int -> Offset
lift = Offset . (`mod` 12)

unary :: (Int -> Int) -> Offset -> Offset
unary f (Offset x) = Offset (f x `mod` 12)

binary :: (Int -> Int -> Int) -> Offset -> Offset -> Offset
binary op (Offset a) (Offset b) = Offset (op a b `mod` 12)

instance Enum Offset where
  succ = unary succ
  pred = unary pred

  fromEnum = mkOffset
  toEnum   = lift

instance Show Offset where
  show = show . mkOffset

instance Num Offset where
  (+) = binary (+)
  (-) = binary (-)
  (*) = binary (*)

  negate (Offset x) = Offset (11 - x)

  signum = const (Offset 1)
  abs    = id

  fromInteger = lift . fromInteger

instance Arbitrary Offset where
  arbitrary = Offset `fmap` choose (0, 11)

addOffset :: Semitone -> Offset -> Semitone
addOffset a (Offset b) = a + fromIntegral b

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt _ _ []     = []
mapAt 0 f (x:xs) = f x : xs
mapAt i f (x:xs) = x : mapAt (i-1) f xs

lower, raise :: Int -> [Offset] -> [Offset]
lower i = mapAt (i-1) (subtract 1)
raise i = mapAt (i+1) (subtract 1)
