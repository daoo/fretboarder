module Fretboarder.Drawing.ASCII where

import Data.List
import Fretboarder.Music.Fretboard
import Fretboarder.Music.INote
import Fretboarder.Music.Note
import Fretboarder.Music.Scale

-- TODO: Use some bytestring builder
asciiFretboard :: Int -> Fretboard -> Scale -> String
asciiFretboard c fb s = intercalate "\n" $ map showString $ tuning fb
  where
    showString :: INote -> String
    showString root = showNut root ++ showNutNote root ++ "|" ++ showFrets root ++ "|"

    showNut :: INote -> String
    showNut n = show (iToNote n) ++ " |"

    showNutNote n = if hasNote n s
      then "X"
      else "|"

    showFrets :: INote -> String
    showFrets root = intercalate "|" $ map showFret [root + 1 .. root + fromIntegral c]

    showFret n = if hasNote n s
      then "-X-"
      else "---"
