module Main (main) where

import Codec.Picture
import Fretboarder.Drawing.Rasterific
import Fretboarder.Fretboard
import Fretboarder.Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  (w : h : path : rest) <- getArgs
  case parseExpressions (concat rest) of
    Left err    -> print err
    Right exprs -> writePng path $ drawFretboard (read w) (read h) ebgdae exprs
