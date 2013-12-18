module Fretboarder.Drawing.ASCII where

import Data.List
import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.INote
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale
import Text.Printf

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
