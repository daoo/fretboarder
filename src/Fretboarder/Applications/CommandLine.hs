--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Data.Char
import Extensions.Tuple
import Fretboarder.Drawing.Backend
import Fretboarder.Drawing.Cairo ()
import Fretboarder.Drawing.Helper
import Fretboarder.Parser.Parser
import Graphics.Rendering.Cairo hiding (scale)
import System.Environment
import System.FilePath

data Type = SVG | PNG

withSurface :: Type -> FilePath -> (Int, Int) -> (Surface -> IO ()) -> IO ()
withSurface SVG file (w, h) r = withSVGSurface file (realToFrac w) (realToFrac h) r
withSurface PNG file (w, h) r = createImageSurface FormatARGB32 w h >>= (\s -> r s >> surfaceWriteToPNG s file)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args = case parseExpr rest of
  Left err   -> print err
  Right expr -> withSurface t file sizeInt $ flip renderWith $ render defaultSettings sizeDouble expr
  where
    (w : h : file : _) = args

    rest = unwords $ drop 3 args

    sizeInt :: (Int, Int)
    sizeInt = mapBoth read (w, h)

    sizeDouble :: Size
    sizeDouble = mapBoth fromIntegral sizeInt

    t = case map toLower $ takeExtension file of
      ".png" -> PNG
      ".svg" -> SVG
      _      -> error "Unknown file type."
