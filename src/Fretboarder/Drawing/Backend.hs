--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Drawing.Backend where

import Extensions.Tuple

import Fretboarder.Drawing.Color
import Fretboarder.Guitar.Fretboard

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

  mapM_ (fillCircle defRadius) $ inlays defInlays
  mapM_ (uncurry (evenPie defRadius)) $ toPoints fb

  return ()
  where
    fretcount   = length (head fb) - 1
    stringcount = length fb
    fretw       = w / realToFrac fretcount
    freth       = h / realToFrac (stringcount - 1)

    -- TODO: Support inlays with more than two dots per fret
    inlays :: [(Int, Int)] -> [Point]
    inlays []            = []
    inlays ((f, 1) : as) = (fretxs !! f, h / 2.0) : inlays as
    inlays ((f, 2) : as) = (fretxs !! f, 3.0 * freth / 2.0) : (fretxs !! f, 7.0 * freth / 2.0) : inlays as
    inlays (_ : as)      = inlays as

    fretxs = 0 : iterate (+ fretw) (fretw / 2.0)
    fretys = iterate (+ freth) 0

    toPoints :: Fretboard -> [(Point, [Color])]
    toPoints = concatMap f . zip fretys . map (zip fretxs)
      where
        f (y, string)      = map (g y) string
        g y (x, Fret _ cs) = ((x, y), cs)

    -- TODO: Make these configurable
    defInlays    = [(3, 1), (5, 1), (7, 1), (9, 1), (12, 2), (15, 1), (17, 1), (19, 1), (21, 1)]
    defRadius    = (h / fromIntegral stringcount) / 5
    defLineWidth = 0.5
