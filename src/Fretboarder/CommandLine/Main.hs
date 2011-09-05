--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Main where

import System
import System.FilePath.Posix

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

-- List of start notes and intervals for scales
makeScales :: Intervals -> Tone -> Accidental -> [(INote, Intervals)]
makeScales s t a = [(toINote (Note t 1 a), s)]

-- Makes lists that can be used for marking a fretboard
makeMarkables :: [(INote, Intervals)] -> [[INote]]
makeMarkables []          = []
makeMarkables ((b, i):ss) = (intervalsToINotes b $ concat $ repeat i) : makeMarkables ss

main :: IO ()
main = do
  -- Args
  args <- getArgs
  let (w:h:file:_) = args
  let rest         = concat $ drop 3 args
  let size         = (read w, read h)
  let t            = case map toLower $ takeExtension file of
                       ".png" -> PNG
                       ".svg" -> SVG
                       _      -> error "Unknown file type."

  let (Parse (PNote tone accidental) (PScale scale)) = parse rest

  let intervals = readIntervals scale
  let scales    = makeScales intervals tone accidental
  let marks     = zip tangoColors $ makeMarkables scales

  let fb       = takeFrets 23 ebgdae
  let fbMarked = markList marks fb
  let fbFinal  = map2 (\ (Fret n c) -> (Fret n (first c))) fbMarked

  withSurface t file size $ renderFretboard (realToFrac $ fst size, realToFrac $ snd size) fbFinal

  where
    first []    = []
    first (x:_) = [x]
