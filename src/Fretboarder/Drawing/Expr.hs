{-# LANGUAGE BangPatterns, LambdaCase #-}
module Fretboarder.Drawing.Expr
  ( Expr(..)
  , semiIndex
  ) where

import Music.Theory.Note
import Music.Theory.SPN
import Music.Theory.Scale.Rooted

-- TODO: Chords, triads, stuff?
data Expr
  = EScale !Rooted
  | ENote !Note
  | EPitch !PitchClass
  deriving Show

includes :: Note -> Expr -> Bool
includes n = \case
  EScale s -> hasNote n s
  ENote n' -> n == n'
  EPitch t -> t == pitchClass (fromNote n)

-- |Returns the indices of the expressions that includes the given note.
semiIndex :: Note -> [Expr] -> [Int]
semiIndex n = go 0 []
  where
    go !_ acc []     = acc
    go !i acc (x:xs) = go (i+1) acc' xs
      where
        acc' = if includes n x then i:acc else acc
