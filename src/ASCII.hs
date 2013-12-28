module Main (main) where

import Data.Attoparsec.Char8
import Data.ByteString.Lazy.Builder (hPutBuilder)
import Fretboarder.Drawing.ASCII
import Fretboarder.Music.Fretboard
import Fretboarder.Parser
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = getArgs >>= prg

prg :: [String] -> IO ()
prg args = case parseOnly parseExprs (B.pack $ unwords args) of
  Left err    -> print err
  Right exprs -> hPutBuilder stdout $ asciiFretboard 23 ebgdae exprs
