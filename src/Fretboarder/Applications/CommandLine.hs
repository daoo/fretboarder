--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Control.Arrow
import Data.Char
import System.Environment
import System.FilePath

import Graphics.Rendering.Cairo hiding (scale)

import Fretboarder.Drawing.Backend
import Fretboarder.Drawing.Cairo ()
import Fretboarder.Drawing.Helper
import Fretboarder.Parser.Parser

data Type = SVG | PNG

withSurface :: Type -> FilePath -> (Int, Int) -> (Surface -> IO ()) -> IO ()
withSurface SVG file (w, h) r = withSVGSurface file (realToFrac w) (realToFrac h) r
withSurface PNG file (w, h) r = createImageSurface FormatARGB32 w h >>= (\s -> r s >> surfaceWriteToPNG s file)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args = case parse rest of
  Ok expr    -> withSurface t file size $ (flip renderWith) $ render defaultSettings ((realToFrac *** realToFrac) size) expr
  Failed err -> putStrLn err
  where
    (w : h : file : _) = args

    rest = unwords $ drop 3 args
    size = (read w, read h)

    t = case map toLower $ takeExtension file of
      ".png" -> PNG
      ".svg" -> SVG
      _      -> error "Unknown file type."
