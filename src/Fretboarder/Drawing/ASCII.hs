{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Fretboarder.Drawing.ASCII (asciiFretboard) where

import Data.List
import Data.Monoid
import Data.ByteString.Builder
import Fretboarder.Drawing.Expr
import Fretboarder.Fretboard
import Music.Theory.Note
import Music.Theory.SPN

buildSPN :: SPN -> Builder
buildSPN (SPN o p) = char8 c <> intDec (fromIntegral o :: Int) <> b
  where
    hash = char8 '#'

    (c, b) = case p of
      Cn -> ('C', mempty)
      Cs -> ('C', hash)
      Dn -> ('D', mempty)
      Ds -> ('D', hash)
      En -> ('E', mempty)
      Fn -> ('F', mempty)
      Fs -> ('F', hash)
      Gn -> ('G', mempty)
      Gs -> ('G', hash)
      An -> ('A', mempty)
      As -> ('A', hash)
      Bn -> ('B', mempty)

buildFret :: Int -> Builder
buildFret = char8 . \case
  0  -> '1'
  1  -> '2'
  2  -> '3'
  3  -> '4'
  4  -> '5'
  5  -> '6'
  6  -> '7'
  7  -> '8'
  8  -> '9'
  9  -> 'A'
  10 -> 'B'
  11 -> 'C'
  12 -> 'D'
  13 -> 'E'
  14 -> 'F'
  _  -> 'e'

-- TODO: Use some bytestring builder
asciiFretboard :: Interval -> Fretboard -> [Expr] -> Builder
asciiFretboard c fb exprs = foldl' (\a n -> a <> string n) mempty $ tuning fb
  where
    indices n = semiIndex n exprs

    string :: Note -> Builder
    string root = nut root <> nutNote root <> pipe <> frets root <> pipe <> newline

    nut :: Note -> Builder
    nut n = buildSPN (fromNote n) <> space <> pipe

    nutNote n = case indices n of
      []    -> pipe
      (x:_) -> buildFret x

    frets :: Note -> Builder
    frets root = foldl' (\a n -> a <> pipe <> fret n) mempty (notes root 1 c)

    fret n = case indices n of
      []    -> dash <> dash <> dash
      (x:_) -> dash <> buildFret x <> dash

    notes root from to = map (root.+) [from..to]

    dash    = char8 '-'
    newline = char8 '\n'
    pipe    = char8 '|'
    space   = char8 ' '
