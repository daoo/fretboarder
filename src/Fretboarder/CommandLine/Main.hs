--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Main where

import System
import System.FilePath.Posix

import Data.Char
import Data.List

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
  let (w:h:file:_) = args
  let rest         = concat $ intersperse " " $ drop 3 args
  let size         = (read w, read h)
  let t            = case map toLower $ takeExtension file of
                       ".png" -> PNG
                       ".svg" -> SVG
                       _      -> error "Unknown file type."

  let (Parse (PNote tone accidental) (PScale scale)) = parse rest

  let offsets = readOffsets scale
  let scale1  = (Scale (toINote (Note tone 1 accidental)) offsets)
  let marks   = zip tangoColors $ [scale1]

  let fb       = takeFrets 23 ebgdae
  let fbMarked = markList marks fb
  let fbFinal  = map2 (\ (Fret n c) -> (Fret n (first c))) fbMarked

  withSurface t file size $ renderFretboard (realToFrac $ fst size, realToFrac $ snd size) fbFinal

  where
    first []    = []
    first (x:_) = [x]

