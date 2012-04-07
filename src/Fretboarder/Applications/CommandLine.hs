--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Control.Arrow
import Data.Char
import System.Environment
import System.FilePath

import Graphics.Rendering.Cairo hiding (scale)

import Extensions.List

import Fretboarder.Drawing.Cairo
import Fretboarder.Drawing.CairoExt
import Fretboarder.Drawing.Color
import Fretboarder.Drawing.Helper
import Fretboarder.Guitar.Fretboard
import Fretboarder.Parser.Parser

data Type = SVG | PNG

withSurface :: Type -> FilePath -> Size -> (Surface -> IO ()) -> IO ()
withSurface SVG file (w, h) r = withSVGSurface file (realToFrac w) (realToFrac h) r
withSurface PNG file (w, h) r = createImageSurface FormatARGB32 w h >>= (\s -> r s >> surfaceWriteToPNG s file)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args = withSurface t file size $ renderFretboard ((realToFrac *** realToFrac) size) fb
  where
    (w : h : file : _) = args

    rest = unwords $ drop 3 args
    size = (read w, read h)

    t = case map toLower $ takeExtension file of
      ".png" -> PNG
      ".svg" -> SVG
      _      -> error "Unknown file type."

    marks = zip tangoColors $ makeList $ makeScales $ parse rest

    fb = map2 f $ markList marks $ takeFrets 23 ebgdae
      where
        f (Fret n c) = Fret n $ headOrEmpty c

