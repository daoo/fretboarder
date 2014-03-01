module Main (main) where

import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO
import Fretboarder.Drawing.ASCII
import Fretboarder.Music.Fretboard
import Fretboarder.Parser
import System.Environment
import System.IO (stdout)

main :: IO ()
main = getArgs >>= prg

prg :: [String] -> IO ()
prg args = case parseExpressions (unwords args) of
  Left err    -> print err
  Right exprs -> hPutStr stdout $ toLazyText $ asciiFretboard 23 ebgdae exprs
