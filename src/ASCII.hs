module Main (main) where

import Data.Attoparsec.Text
import Data.List
import Fretboarder.Drawing.ASCII
import Fretboarder.Music.Fretboard
import Fretboarder.Parser
import System.Environment
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  case parseOnly parseExprs (T.pack $ intercalate " " args) of
    Left err    -> print err
    Right exprs -> putStrLn $ asciiFretboard 23 ebgdae exprs
