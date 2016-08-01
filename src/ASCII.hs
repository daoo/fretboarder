{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.ByteString.Builder
import Fretboarder.Drawing.ASCII
import Fretboarder.Drawing.Expr
import Fretboarder.Fretboard
import Fretboarder.Parser
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = getArgs >>= \case
  "-h":_      -> help >>= putStrLn
  "--help":_  -> help >>= putStrLn
  exprs@(_:_) -> either die printBoard (parseExpressions (unwords exprs))
  _           -> help >>= die

printBoard :: [Expr] -> IO ()
printBoard = hPutBuilder stdout . asciiFretboard 23 ebgdae

help :: IO String
help = do
  name <- getProgName
  return $ "Usage: " ++ name ++ " \"EXPR(,...)\""
