module Fretboarder.Drawing.ASCII where

import Data.List
import Fretboarder.Music.Fretboard
import Fretboarder.Music.Note
import Fretboarder.Music.SPN
import Fretboarder.Music.Scale
import Fretboarder.Music.Semitone

-- TODO: Use some bytestring builder
asciiFretboard :: Int -> Fretboard -> Scale -> String
asciiFretboard c fb s = intercalate "\n" $ map showString $ tuning fb
  where
    showString :: Semitone -> String
    showString root = showNut root ++ showNutNote root ++ "|" ++ showFrets root ++ "|"

    showNut :: Semitone -> String
    showNut n = show (fromSemi n :: SPN) ++ " |"

    showNutNote n = if hasNote n s
      then "X"
      else "|"

    showFrets :: Semitone -> String
    showFrets root = intercalate "|" $ map showFret [root + 1 .. root + fromIntegral c]

    showFret n = if hasNote n s
      then "-X-"
      else "---"
