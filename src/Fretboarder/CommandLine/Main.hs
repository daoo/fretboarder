module Main where

import Data.List (nub)
import Graphics.Rendering.Cairo

import Fretboarder.Drawing.Cairo
import Fretboarder.Drawing.Color

import Fretboarder.Extensions.List

import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.INote
import Fretboarder.Guitar.Scale
import Fretboarder.Guitar.Note

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
  withSVGSurface "test.svg" w h $ renderFretboard size fbFinal
  where
    scale      = dorianMode
    tone       = E
    accidental = Natural

    scales = makeScales scale tone accidental
    marks  = zip colors $ makeMarkables scales

    fb       = takeFrets 23 ebgdae
    fbMarked = markFretboard marks fb
    fbFinal  = map2 (\ (Fret n c) -> (Fret n (first c))) fbMarked
      where
        first []     = []
        first (x:xs) = [x]

    size@(w, h) = (1280, 300)

