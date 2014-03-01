{-# LANGUAGE OverloadedStrings #-}
module Fretboarder.Drawing.ASCII (asciiFretboard) where

import Data.List
import Data.Monoid
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Fretboarder.Drawing.Expr
import Fretboarder.Music.Fretboard
import Music.Theory.Note
import Music.Theory.SPN

buildSPN :: SPN -> Builder
buildSPN (SPN o p) = singleton c <> decimal (fromIntegral o :: Int) <> b
  where
    hash = singleton '#'

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

chars :: [Builder]
chars = map singleton "123456789ABCDEF"

getChars :: Note -> [Expr] -> [Builder]
getChars n = map (chars !!) . semiIndex n

-- TODO: Use some bytestring builder
asciiFretboard :: Int -> Fretboard -> [Expr] -> Builder
asciiFretboard c fb exprs = foldl' (\a n -> a <> string n) mempty $ tuning fb
  where
    string :: Note -> Builder
    string root = nut root <> nutNote root <> pipe <> frets root <> pipe <> newline

    nut :: Note -> Builder
    nut n = buildSPN (fromNote n) <> space <> pipe

    nutNote n = case getChars n exprs of
      []    -> pipe
      (x:_) -> x

    frets :: Note -> Builder
    frets root = foldl' (\a n -> a <> pipe <> fret n) mempty [root + toEnum 1 .. root + toEnum c]

    fret n = case getChars n exprs of
      []    -> dash <> dash <> dash
      (x:_) -> dash <> x <> dash

    dash    = singleton '-'
    newline = singleton '\n'
    pipe    = singleton '|'
    space   = singleton ' '
