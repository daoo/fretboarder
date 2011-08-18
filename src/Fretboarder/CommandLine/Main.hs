module Main where

import System
import System.FilePath.Posix

import Data.Char
import Data.List (nub)

import Graphics.Rendering.Cairo

import Fretboarder.Drawing.Cairo
import Fretboarder.Drawing.CairoExt
import Fretboarder.Drawing.Color

import Fretboarder.Extensions.List

import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.INote
import Fretboarder.Guitar.Scale
import Fretboarder.Guitar.Note

import Fretboarder.Parser.Parser

data Type = SVG | PNG

withSurface :: Type -> FilePath -> Size -> (Surface -> IO a) -> IO a
withSurface SVG file (w, h) r = withSVGSurface file w h r
withSurface PNG file _ r      = withImageSurfaceFromPNG file r

-- List of start notes and intervals for scales
makeScales :: Scale -> Tone -> Accidental -> [(INote, Scale)]
makeScales s t a = [(toINote (Note t 1 a), s)]

-- Makes lists that can be used for marking a fretboard
-- TODO: take 30 is a stupid hack, make use of some awesome laziness instead
makeMarkables :: [(INote, Scale)] -> [[INote]]
makeMarkables []          = []
makeMarkables ((b, i):ss) = (nub $ makeScale b $ concat $ take 30 $ repeat i) : makeMarkables ss

main :: IO ()
main = do
  -- Args
  args <- getArgs
  let (w:h:file:_) = args
  let scale      = concat $ drop 3 args
  let size       = (read w, read h)
  let t          = case map toLower $ takeExtension file of
                     ".png" -> PNG
                     ".svg" -> SVG
                     _      -> error "Unknown file type."

  let (Parse (PNote tone accidental)) = parse scale

  let scale = dorianMode

  let scales = makeScales scale tone accidental
  let marks  = zip colors $ makeMarkables scales

  let fb       = takeFrets 23 ebgdae
  let fbMarked = markFretboard marks fb
  let fbFinal  = map2 (\ (Fret n c) -> (Fret n (first c))) fbMarked

  withSurface t file size $ renderFretboard size fbFinal

  where
    first []     = []
    first (x:xs) = [x]
