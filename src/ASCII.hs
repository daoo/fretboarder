module Main (main) where

import Data.ByteString.Builder (hPutBuilder)
import Fretboarder.Drawing.ASCII
import Fretboarder.Fretboard
import Fretboarder.Parser
import System.Environment
import System.IO (stdout)

main :: IO ()
main = getArgs >>= prg

prg :: [String] -> IO ()
prg args = case parseExpressions (unwords args) of
  Left err    -> print err
  Right exprs -> hPutBuilder stdout $ asciiFretboard 23 ebgdae exprs
