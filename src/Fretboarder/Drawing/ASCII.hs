module Fretboarder.Drawing.ASCII (asciiFretboard) where

import Data.List
import Fretboarder.Drawing.Expr
import Fretboarder.Music.Fretboard
import Fretboarder.Music.SPN
import Fretboarder.Music.Semitone

chars :: String
chars = "123456789ABCDEF"

getChars :: Semitone -> [Expr] -> String
getChars n = map (chars !!) . semiIndex n

-- TODO: Use some bytestring builder
asciiFretboard :: Int -> Fretboard -> [Expr] -> String
asciiFretboard c fb exprs = intercalate "\n" $ map showString $ tuning fb
  where
    showString :: Semitone -> String
    showString root = showNut root ++ showNutNote root ++ "|" ++ showFrets root ++ "|"

    showNut :: Semitone -> String
    showNut n = show (fromSemi n :: SPN) ++ " |"

    showNutNote n = case getChars n exprs of
      []    -> "|"
      (x:_) -> [x]

    showFrets :: Semitone -> String
    showFrets root = intercalate "|" $ map showFret [root + 1 .. root + fromIntegral c]

    showFret n = case getChars n exprs of
      []    -> "---"
      (x:_) -> ['-', x, '-']
