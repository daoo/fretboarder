module Fretboarder.Drawing.ASCII (asciiFretboard) where

import Data.List
import Fretboarder.Drawing.Expr
import Fretboarder.Music.Fretboard
import Fretboarder.Music.Note
import Fretboarder.Music.SPN

chars :: String
chars = "123456789ABCDEF"

getChars :: Note -> [Expr] -> String
getChars n = map (chars !!) . semiIndex n

-- TODO: Use some bytestring builder
asciiFretboard :: Int -> Fretboard -> [Expr] -> String
asciiFretboard c fb exprs = intercalate "\n" $ map string $ tuning fb
  where
    string :: Note -> String
    string root = nut root ++ nutNote root ++ "|" ++ frets root ++ "|"

    nut :: Note -> String
    nut n = show (fromSemi n :: SPN) ++ " |"

    nutNote n = case getChars n exprs of
      []    -> "|"
      (x:_) -> [x]

    frets :: Note -> String
    frets root = intercalate "|" $ map fret [root + toEnum 1 .. root + toEnum c]

    fret n = case getChars n exprs of
      []    -> "---"
      (x:_) -> ['-', x, '-']
