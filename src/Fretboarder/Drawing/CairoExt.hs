--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Drawing.CairoExt where

import Fretboarder.Extensions.Tuple

import Graphics.Rendering.Cairo

import Fretboarder.Drawing.Color

type Point = (Double, Double)
type Size  = (Int, Int)
type Line  = (Point, Point)

pi2 :: Double
pi2 = pi * 2.0

evenPie :: Point -> Double -> [Color] -> Render ()
evenPie (x, y) rad colors = helper 0 colors
  where
    helper :: Double -> [Color] -> Render ()
    helper _ []             = return ()
    helper a ((r, g, b):cs) = setSourceRGB r g b >> arc x y rad a a' >> lineTo x y >> fill >> helper a' cs
      where
        a' = a + delta

    delta :: Double
    delta = pi2 / realToFrac (length colors)

circle :: Double -> Point -> Render ()
circle r (x, y) = moveTo x y >> arc x y r 0 pi2

line :: Line -> Render ()
line ((x1, y1), (x2, y2)) = moveTo x1 y1 >> lineTo x2 y2

deltaLines :: Int -> Point -> Line -> Render [()]
deltaLines count delta first = mapM line $ take count $ iterate (mapBoth (+++ delta)) first

(+++) :: Point -> Point -> Point
(x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)

