{-# LANGUAGE LambdaCase #-}
module Fretboarder.Drawing.Expr
  ( Expr(..)
  , semiIndex
  ) where

import Fretboarder.Music.Offset
import Fretboarder.Music.Scale
import Fretboarder.Music.Semitone

data Expr = FullScale Scale
          | AllTones Offset
          | OnePitch Semitone
          -- Chords, triads, stuff?

semiIndex :: Semitone -> [Expr] -> [Int]
semiIndex n = go 0 []
  where
    go _ acc []     = acc
    go i acc (x:xs) = go (i+1) acc' xs
      where
        acc' = if f x then i:acc else acc

    f = \case
      FullScale s -> hasNote n s
      AllTones _  -> undefined
      OnePitch n' -> n == n'
