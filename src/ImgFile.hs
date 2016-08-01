{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Codec.Picture
import Fretboarder.Drawing.Expr
import Fretboarder.Drawing.Rasterific
import Fretboarder.Fretboard
import Fretboarder.Parser
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= \case
  "-h":_                  -> help >>= putStrLn
  "--help":_              -> help >>= putStrLn
  width:height:path:exprs -> program (read width) (read height) path (concat exprs)
  _                       -> help >>= die

program :: Int -> Int -> FilePath -> String -> IO ()
program width height path exprs =
  either die (outputBoard width height path) (parseExpressions exprs)

outputBoard :: Int -> Int -> FilePath -> [Expr] -> IO ()
outputBoard width height path = writePng path . drawFretboard width height ebgdae

help :: IO String
help = do
  name <- getProgName
  return $ "Usage: " ++ name ++ " WIDTH HEIGHT PATH.PNG \"EXPR(,...)\""
