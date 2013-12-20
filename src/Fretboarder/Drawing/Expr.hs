{-# LANGUAGE LambdaCase #-}
module Fretboarder.Drawing.Expr
  ( Expr(..)
  , semiIndex
  ) where

import Fretboarder.Music.Note
import Fretboarder.Music.RootedScale

data Expr = FullScale RootedScale
          | OnePitch Note
          | OneTone ScaleOffset
          -- Chords, triads, stuff?

includes :: Note -> Expr -> Bool
includes n = \case
  FullScale s -> hasNote n s
  OnePitch n' -> n == n'
  OneTone t   -> t == scaleOffset n

semiIndex :: Note -> [Expr] -> [Int]
semiIndex n = go 0 []
  where
    go _ acc []     = acc
    go i acc (x:xs) = go (i+1) acc' xs
      where
        acc' = if includes n x then i:acc else acc
