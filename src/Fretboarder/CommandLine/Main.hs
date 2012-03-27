--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Main where

import System.Environment
import System.FilePath.Posix

import Control.Arrow

import Data.Char

import Graphics.Rendering.Cairo hiding (scale)

import Fretboarder.Drawing.Cairo
import Fretboarder.Drawing.CairoExt
import Fretboarder.Drawing.Color
import Fretboarder.Extensions.List
import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale
import Fretboarder.Parser.Parser
import Fretboarder.Parser.String

data Type = SVG | PNG

withSurface :: Type -> FilePath -> Size -> (Surface -> IO ()) -> IO ()
withSurface SVG file (w, h) r = withSVGSurface file (realToFrac w) (realToFrac h) r
withSurface PNG file (w, h) r = do
  s <- createImageSurface FormatARGB32 w h
  r s
  surfaceWriteToPNG s file

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run args = withSurface t file size $ renderFretboard ((realToFrac *** realToFrac) size) fbFinal
  where
    (w:h:file:_) = args
    rest         = unwords $ drop 3 args
    size         = (read w, read h)
    t            = case map toLower $ takeExtension file of
                     ".png" -> PNG
                     ".svg" -> SVG
                     _      -> error "Unknown file type."

    (Parse (PNote tone accidental) (PScale scale)) = parse rest

    offsets = readOffsets scale
    scale1  = Scale (toINote (Note tone 1 accidental)) offsets
    marks   = zip tangoColors [scale1]

    fb       = takeFrets 23 ebgdae
    fbMarked = markList marks fb
    fbFinal  = map2 (\ (Fret n c) -> (Fret n (firstOrEmpty c))) fbMarked

    firstOrEmpty []    = []
    firstOrEmpty (x:_) = [x]

