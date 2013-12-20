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
asciiFretboard c fb exprs = intercalate "\n" $ map showString $ tuning fb
  where
    showString :: Note -> String
    showString root = showNut root ++ showNutNote root ++ "|" ++ showFrets root ++ "|"

    showNut :: Note -> String
    showNut n = show (fromSemi n :: SPN) ++ " |"

    showNutNote n = case getChars n exprs of
      []    -> "|"
      (x:_) -> [x]

    showFrets :: Note -> String
    showFrets root = intercalate "|" $ map showFret [root + toEnum 1 .. root + toEnum c]

    showFret n = case getChars n exprs of
      []    -> "---"
      (x:_) -> ['-', x, '-']
