{-# LANGUAGE BangPatterns, LambdaCase #-}
module Fretboarder.Drawing.Expr
  ( Expr(..)
  , semiIndex
  ) where

import Music.Theory.Note
import Music.Theory.Scale.Rooted

data Expr = FullScale !Rooted
          | OnePitch !Note
          | OneTone !ScaleOffset
          -- Chords, triads, stuff?

  deriving Show

includes :: Note -> Expr -> Bool
includes n = \case
  FullScale s -> hasNote n s
  OnePitch n' -> n == n'
  OneTone t   -> t == scaleOffset n

semiIndex :: Note -> [Expr] -> [Int]
semiIndex n = go 0 []
  where
    go !_ acc []     = acc
    go !i acc (x:xs) = go (i+1) acc' xs
      where
        acc' = if includes n x then i:acc else acc
