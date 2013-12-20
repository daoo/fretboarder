module Main (main) where

import Data.Attoparsec.Text
import Fretboarder.Drawing.ASCII
import Fretboarder.Music.Fretboard
import Fretboarder.Parser
import System.Environment
import qualified Data.Text as T

main :: IO ()
main = getArgs >>= prg

prg :: [String] -> IO ()
prg args = case parseOnly parseExprs (T.pack $ unwords args) of
  Left err    -> print err
  Right exprs -> putStrLn $ asciiFretboard 23 ebgdae exprs
