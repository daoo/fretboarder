module Cairo where

import CairoExt
import Colors
import Data.List (nub)
import Data.Ratio
import Graphics.Rendering.Cairo

import Note
import Intervals
import Scale
import Fretboard

drawFretboard :: Size -> Fretboard -> Render ()
drawFretboard (w, h) fb = do
  save
  translate px py

  -- Draw the thick nut line
  setLineWidth (defLineWidth * 10.0)
  moveTo 0 0
  lineTo 0 boardh
  stroke
  setLineWidth defLineWidth

  newPath

  -- Draw frets and strings
  deltaLines fretcount (fretw, 0) ((fretw, 0), (fretw, boardh))
  deltaLines stringcount (0, freth) ((0, 0), (boardw, 0))

  stroke

  -- Draw inlays
  mapM (circle defRadius) $ inlays defInlays 
  fill

  -- Draw fretboard
  draw fb

  return ()

  where
    aspect           = w / h
    padding          = h * 0.2
    (px, py)         = (padding, padding)
    (boardw, boardh) = (w - padding * 2.0, h - padding * 2.0)
    (fretw, freth)   = (boardw / realToFrac fretcount, boardh / realToFrac (stringcount - 1))

    fretcount   = (length $ head fb) - 1
    stringcount = length fb

    inlays :: [(Int, Int)] -> [Point]
    inlays []          = []
    inlays ((f, 1):as) = (fretx f, boardh / 2.0) : inlays as
    inlays ((f, 2):as) = (fretx f, 3.0 * freth / 2.0) : ( fretx f, 7.0 * freth / 2.0 ) : inlays as

    fretx :: Int -> Double
    fretx i = fretw * (realToFrac i) - (fretw / 2.0)

    draw :: Fretboard -> Render ()
    draw fb = helper (-fretw / 2.0, 0) fb
      where
        helper :: Point -> Fretboard -> Render ()
        helper _ []     = return ()
        helper p (a:as) = drawString p a >> helper (p +++ delta) as

        delta = (0, freth)

    drawString :: Point -> GuitarString -> Render ()
    drawString _ []             = return ()
    drawString pt@(x, y) (f:fs) = drawFret pt f >> drawString (pt +++ delta) fs
      where
        delta = (fretw, 0)

    drawFret :: Point -> Fret -> Render ()
    drawFret pt (Fret n colors) = evenPie pt defRadius colors

    -- TODO: Make these configurable
    defInlays    = zip [ 3, 5, 7, 9, 15, 17, 19, 21 ] (repeat 1) ++ zip [ 12 ] (repeat 2)
    defRadius    = 7
    defLineWidth = 0.5

main :: IO ()
main = do
  withSVGSurface "test.svg" w h (\ s -> renderWith s $ drawFretboard (w, h) exfb)

  where
    w = 1280
    h = 300

exfb = (map . map) (\ (Fret n c) -> (Fret n (f c))) $ markFretboard markables $ takeFrets 23 ebgdae
  where
    f []     = []
    f (x:xs) = [x]

markFretboard :: [(Fretboard.Color, Scale)] -> Fretboard -> Fretboard
markFretboard [] fb           = fb
markFretboard ((c, is):ls) fb = markScale c is fb'
  where
    fb' = markFretboard ls fb

scales = [ (toINote (Note A 1 Natural), harmonicMinor) ]

intvs []          = []
intvs ((b, i):ss) = (nub $ makeScale b $ concat $ take 30 $ repeat i) : intvs ss
    
markables = zip colors (intvs scales)

