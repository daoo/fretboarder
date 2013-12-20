{-# LANGUAGE LambdaCase #-}
module Fretboarder.Drawing.Expr
  ( Expr(..)
  , semiIndex
  ) where

import Fretboarder.Music.RootedScale
import Fretboarder.Music.Note

data Expr = FullScale RootedScale
          | OnePitch Note
          -- Chords, triads, stuff?

includes :: Note -> Expr -> Bool
includes n = \case
  FullScale s -> hasNote n s
  OnePitch n' -> n == n'

semiIndex :: Note -> [Expr] -> [Int]
semiIndex n = go 0 []
  where
    go _ acc []     = acc
    go i acc (x:xs) = go (i+1) acc' xs
      where
        acc' = if includes n x then i:acc else acc
