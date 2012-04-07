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
import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale
import Fretboarder.Parser.Parser
import Fretboarder.Parser.String

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

    ParseScale (PScale (PNote tone accidental) scale) = parse rest

    offsets = readOffsets scale
    scale1  = Scale (toINote (Note tone 1 accidental)) offsets
    marks   = zip tangoColors [scale1]

    fb = map2 f $ markList marks $ takeFrets 23 ebgdae
      where
        f (Fret n c) = Fret n $ headOrEmpty c

