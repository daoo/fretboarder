module Cairo where

import Data.Ratio
import Graphics.Rendering.Cairo

import Note
import Intervals
import Scale
import Fretboard

type Point = (Double, Double)
type Size  = (Double, Double)
type Line  = (Point, Point)

(+++) :: Point -> Point -> Point
(x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)

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
  drawLines fretcount (fretw, 0) ((fretw, 0), (fretw, boardh))
  drawLines stringcount (0, freth) ((0, 0), (boardw, 0))

  stroke

  -- Draw inlays
  mapM (drawCircle defRadius) $ inlays defInlays 
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

    fretcount   = length $ head fb
    stringcount = length fb

    inlays :: [(Int, Int)] -> [Point]
    inlays []          = []
    inlays ((f, 1):as) = (fretx f, boardh / 2.0) : inlays as
    inlays ((f, 2):as) = (fretx f, 3.0 * freth / 2.0) : ( fretx f, 7.0 * freth / 2.0 ) : inlays as

    fretx :: Int -> Double
    fretx i = fretw * (realToFrac i) - (fretw / 2.0)

    drawLine :: Line -> Render ()
    drawLine ((x1, y1), (x2, y2)) = moveTo x1 y1 >> lineTo x2 y2

    drawLines :: Int -> Point -> Line -> Render [()]
    drawLines count delta first = mapM drawLine $ take count $ iterate (mapBoth (+++ delta)) first

    drawCircle :: Double -> Point -> Render ()
    drawCircle r (x, y) = moveTo x y >> arc x y r 0 (2.0 * pi)

    draw :: Fretboard -> Render ()
    draw fb = helper (-fretw / 2.0, 0) fb
      where
        helper :: Point -> Fretboard -> Render ()
        helper _ []     = return ()
        helper p (a:as) = drawString p a >> helper (p +++ delta) as

        delta = (0, freth)

    drawString :: Point -> GuitarString -> Render ()
    drawString p gs = helper p gs
      where
        helper :: Point -> [Fret] -> Render ()
        helper _ []     = return ()
        helper p (a:as) = drawFret p a >> helper (p +++ (fretw, 0)) as

    drawFret :: Point -> Fret -> Render ()
    drawFret (x, y) (Fret n cs) = helper 0 cs
      where
        helper :: Double -> [Color] -> Render ()
        helper _ []             = return ()
        helper a ((r, g, b):cs) = withRGBPattern r g b (\ _ -> arc x y defRadius a (a + delta) >> fill )

        delta :: Double
        delta = (2.0 * pi) / ( realToFrac $ length cs )

    -- TODO: Make these configurable
    defInlays    = zip [ 3, 5, 7, 9, 15, 17, 19 ] (repeat 1) ++ zip [ 12 ] (repeat 2)
    defRadius    = 7
    defLineWidth = 0.5

test :: IO ()
test = do
  withSVGSurface "test.svg" 2000 200 (\ s -> renderWith s $ drawFretboard (1280.0, 200.0) fb)

  where
    fb = markScale (1, 0, 0) s $ takeFrets 20 ebgdae
    s = makeScale (Note A 3 Natural) pentatonicIntervals

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)
