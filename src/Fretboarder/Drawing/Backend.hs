--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Drawing.Backend where

import Extensions.Tuple

import Fretboarder.Guitar.Fretboard

type Color = (Double, Double, Double)
type Point = (Double, Double)
type Size  = (Double, Double)
type Line  = (Point, Point)

class (Monad a) => Backend a where
  setColor :: Color -> a ()
  setLineWidth :: Double -> a ()

  strokeLines :: [Line] -> a ()
  strokeRectangle :: Point -> Size -> a ()
  fillRectangle :: Point -> Size -> a ()
  fillCircle :: Double -> Point -> a ()
  fillArcs :: Double -> Point -> [(Color, (Double, Double))] -> a ()

deltaLines :: Backend a => Int -> Point -> Line -> a ()
deltaLines count delta = strokeLines . take count . iterate (mapBoth (+++ delta))

evenPie :: Backend a => Double -> Point -> [Color] -> a ()
evenPie r p colors = fillArcs r p $ zip colors (zip angles (tail angles))
  where
    angles = [0, pi2 / realToFrac (length colors) .. ]

(+++) :: Point -> Point -> Point
(x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)

pi2 :: Double
pi2 = pi * 2.0

drawFretboard :: Backend a => Size -> Fretboard -> a ()
drawFretboard (w, h) fb = do
  setColor (255, 255, 255)
  fillRectangle (0, 0) (w, h)
  setColor (0, 0, 0)

  setLineWidth $ defLineWidth * 10.0
  strokeLines [((0, 0), (0, h))]
  setLineWidth defLineWidth

  deltaLines fretcount (fretw, 0) ((fretw, 0), (fretw, h))
  deltaLines stringcount (0, freth) ((0, 0), (w, 0))

  _ <- mapM (fillCircle defRadius) $ inlays defInlays
  draw fb

  return ()
  where
    fretcount   = length (head fb) - 1
    stringcount = length fb
    fretw       = w / realToFrac fretcount
    freth       = h / realToFrac (stringcount - 1)

    -- TODO: Support inlays with more than two dots per fret
    inlays :: [(Int, Int)] -> [Point]
    inlays []            = []
    inlays ((f, 1) : as) = (fretx f, h / 2.0) : inlays as
    inlays ((f, 2) : as) = (fretx f, 3.0 * freth / 2.0) : (fretx f, 7.0 * freth / 2.0) : inlays as
    inlays (_ : as)      = inlays as

    fretx :: Int -> Double
    fretx i = fretw * realToFrac i - (fretw / 2.0)

    draw :: Backend a => Fretboard -> a ()
    draw = mapM_ (uncurry drawString) . zip points
      where
        points = iterate (+++ (0, freth)) (-fretw / 2.0, 0)

    drawString :: Backend a => Point -> GuitarString -> a ()
    drawString _ []      = return ()
    drawString pt (f:fs) = drawFret pt f >> drawString (pt +++ delta) fs
      where
        delta = (fretw, 0)

    drawFret :: Backend a => Point -> Fret -> a ()
    drawFret pt (Fret _ colors) = evenPie defRadius pt colors

    -- TODO: Make these configurable
    defInlays    = [(3, 1), (5, 1), (7, 1), (9, 1), (12, 2), (15, 1), (17, 1), (19, 1), (21, 1)]
    defRadius    = (h / fromIntegral stringcount) / 3
    defLineWidth = 0.5
