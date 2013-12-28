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
buildSPN (SPN o p) = case p of
  Cn -> charUtf8 'C' <> intDec (fromIntegral o)
  Cs -> charUtf8 'C' <> intDec (fromIntegral o) <> charUtf8 '#'
  Dn -> charUtf8 'D' <> intDec (fromIntegral o)
  Ds -> charUtf8 'D' <> intDec (fromIntegral o) <> charUtf8 '#'
  En -> charUtf8 'E' <> intDec (fromIntegral o)
  Fn -> charUtf8 'F' <> intDec (fromIntegral o)
  Fs -> charUtf8 'F' <> intDec (fromIntegral o) <> charUtf8 '#'
  Gn -> charUtf8 'G' <> intDec (fromIntegral o)
  Gs -> charUtf8 'G' <> intDec (fromIntegral o) <> charUtf8 '#'
  An -> charUtf8 'A' <> intDec (fromIntegral o)
  As -> charUtf8 'A' <> intDec (fromIntegral o) <> charUtf8 '#'
  Bn -> charUtf8 'B' <> intDec (fromIntegral o)

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
