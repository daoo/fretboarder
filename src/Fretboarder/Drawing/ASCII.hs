{-# LANGUAGE OverloadedStrings #-}
module Fretboarder.Drawing.ASCII (asciiFretboard) where

import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.List
import Data.Monoid
import Fretboarder.Drawing.Expr
import Fretboarder.Music.Fretboard
import Fretboarder.Music.Note
import Fretboarder.Music.SPN

buildSPN :: SPN -> Builder
buildSPN (SPN o p) = charUtf8 c <> intDec (fromIntegral o) <> b
  where
    hash = charUtf8 '#'

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
chars = map charUtf8 "123456789ABCDEF"

getChars :: Note -> [Expr] -> [Builder]
getChars n = map (chars !!) . semiIndex n

-- TODO: Use some bytestring builder
asciiFretboard :: Int -> Fretboard -> [Expr] -> Builder
asciiFretboard c fb exprs = foldl' (\a n -> a <> string n) mempty $ tuning fb
  where
    string :: Note -> Builder
    string root = nut root <> nutNote root <> pipe <> frets root <> pipe <> newline

    nut :: Note -> Builder
    nut n = buildSPN (fromSemi n) <> space <> pipe

    nutNote n = case getChars n exprs of
      []    -> pipe
      (x:_) -> x

    frets :: Note -> Builder
    frets root = foldl' (\a n -> a <> pipe <> fret n) mempty [root + toEnum 1 .. root + toEnum c]

    fret n = case getChars n exprs of
      []    -> dash <> dash <> dash
      (x:_) -> dash <> x <> dash

    dash    = char8 '-'
    newline = char8 '\n'
    pipe    = char8 '|'
    space   = char8 ' '
