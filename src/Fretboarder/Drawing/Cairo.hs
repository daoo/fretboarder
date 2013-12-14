{-# LANGUAGE BangPatterns, LambdaCase #-}
module Fretboarder.Drawing.Cairo ( drawFretboard ) where

import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Scale
import Fretboarder.Utility
import qualified Graphics.Rendering.Cairo as C

type Color = (Double, Double, Double)
type Point = (Double, Double)
type Size  = (Double, Double)
type Line  = (Point, Point)

{-tangoColors :: [Color]-}
{-tangoColors = [ (0.988235294117647, 0.917647058823529, 0.309803921568627)-}
              {-, (0.937254901960784, 0.160784313725490, 0.16078431372549)-}
              {-, (0.988235294117647, 0.686274509803922, 0.243137254901961)-}
              {-, (0.678431372549020, 0.498039215686275, 0.658823529411765)-}
              {-, (0.447058823529412, 0.623529411764706, 0.811764705882353)-}
              {-, (0.541176470588235, 0.886274509803922, 0.203921568627451)-}
              {-, (0.913725490196078, 0.725490196078431, 0.431372549019608)-}
              {-, (0.533333333333333, 0.541176470588235, 0.52156862745098)-}
              {-]-}

(+++) :: Point -> Point -> Point
(x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)

pi2 :: Double
pi2 = pi * 2.0

setColor :: Color -> C.Render ()
setColor (r, g, b) = C.setSourceRGB r g b

{-strokeRectangle :: Point -> Size -> C.Render ()-}
{-strokeRectangle (x, y) (w, h) = C.rectangle x y w h >> C.stroke-}

fillRectangle :: Point -> Size -> C.Render ()
fillRectangle (x, y) (w, h) = C.rectangle x y w h >> C.fill

strokeLines :: [Line] -> C.Render ()
strokeLines ls = mapM_ line ls >> C.stroke

fillCircle :: Double -> Point -> C.Render ()
fillCircle r (x, y)  = C.arc x y r 0 pi2 >> C.fill

{-fillArcs :: Double -> Point -> [(Color, (Double, Double))] -> C.Render ()-}
{-fillArcs r (x, y) cs = mapM_ f cs >> C.fill-}
  {-where-}
    {-f (c, (a, b)) = setColor c >> C.arc x y r a b-}

line :: Line -> C.Render ()
line ((x1, y1), (x2, y2)) = C.moveTo x1 y1 >> C.lineTo x2 y2

deltaLines :: Int -> Point -> Line -> C.Render ()
deltaLines count delta = strokeLines . take count . iterate (mapBoth (+++ delta))

{-evenPie :: Double -> Point -> [Color] -> C.Render ()-}
{-evenPie r p colors = fillArcs r p $ zip colors (zip angles (tail angles))-}
  {-where-}
    {-angles = [0, pi2 / fromIntegral (length colors) .. ]-}

drawFretboard :: Size -> Fretboard -> [Scale] -> C.Render ()
drawFretboard (w, h) fb scales = do
  setColor bgColor
  fillRectangle (0, 0) (w, h)
  setColor fgColor

  C.setLineWidth (lineWidth * 10.0)
  line (topLeft, (px, py + bh))
  C.setLineWidth lineWidth

  deltaLines frets (fretw, 0) ((px + fretw, py), (px + fretw, py + bh))
  deltaLines strings (0, freth) (topLeft, (px + bw, py))

  drawInlays inlays
  drawFrets
  where
    lineWidth = 1.0
    fgColor   = (0, 0, 0)
    bgColor   = (255, 255, 255)

    frets   = 23
    strings = stringCount fb

    inlays = [(3, 1), (5, 1), (7, 1), (9, 1), (12, 2), (15, 1), (17, 1), (19, 1), (21, 1)]

    topLeft@(px, py) = (50, 20)

    bw = w - 2 * px
    bh = h - 2 * py

    fretw  = bw / fromIntegral frets
    freth  = bh / fromIntegral (strings - 1)
    radius = (bh / fromIntegral strings) / 5

    fretx, frety :: Int -> Double
    fretx 0 = px
    fretx i = px + fretw / 2.0 + realToFrac (i - 1) * fretw
    frety i = py + realToFrac i * freth

    circle = fillCircle radius

    -- TODO: Support inlays with more than two dots per fret
    drawInlay :: (Int, Int) -> C.Render ()
    drawInlay (f, 1) = let x = fretx f in circle (x, py + bh / 2.0)
    drawInlay (f, 2) = let x = fretx f in circle (x, py + 3.0 * freth / 2.0) >> circle (x, py + 7.0 * freth / 2.0)
    drawInlay _      = return ()

    drawInlays = mapM_ drawInlay

    drawFret i j = circle (fretx i, frety j)

    drawFrets = goy 0
      where
        goy !j | j < strings = gox 0 >> goy (j+1)
               | otherwise   = return ()

          where
            gox !i | i <= frets = drawFret i j >> gox (i+1)
                   | otherwise  = return ()
