{-# LANGUAGE BangPatterns, LambdaCase #-}
module Fretboarder.Drawing.Cairo
  ( drawFretboard ) where

import Control.Applicative
import Data.List
import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale
import qualified Graphics.Rendering.Cairo as C

data Color = Color !Double !Double !Double
data Point = Point !Double !Double
data Line  = Line !Point !Point

instance Num Point where
  Point x1 y1 + Point x2 y2 = Point (x1+x2) (y1+y2)

  (*) = undefined
  (-) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

tangoColors :: [Color]
tangoColors =
  [ Color 0.988235294117647 0.917647058823529 0.309803921568627
  , Color 0.937254901960784 0.160784313725490 0.160784313725490
  , Color 0.988235294117647 0.686274509803922 0.243137254901961
  , Color 0.678431372549020 0.498039215686275 0.658823529411765
  , Color 0.447058823529412 0.623529411764706 0.811764705882353
  , Color 0.541176470588235 0.886274509803922 0.203921568627451
  , Color 0.913725490196078 0.725490196078431 0.431372549019608
  , Color 0.533333333333333 0.541176470588235 0.521568627450980
  ]

(++++) :: Line -> Point -> Line
Line a b ++++ p = Line (a + p) (b + p)

pi2 :: Double
pi2 = pi * 2.0

setColor :: Color -> C.Render ()
setColor (Color r g b) = C.setSourceRGB r g b

mkCircle :: Double -> Point -> C.Render ()
mkCircle r (Point x y) = C.arc x y r 0 pi2

mkLine :: Line -> C.Render ()
mkLine (Line (Point x1 y1) (Point x2 y2)) = C.moveTo x1 y1 >> C.lineTo x2 y2

mkLines :: Int -> Point -> Line -> C.Render ()
mkLines !count !delta !line = go 0 line
  where
    go !i !acc | i < count = mkLine acc >> go (i+1) (acc ++++ delta)
               | otherwise = return ()

getColor :: INote -> [Scale] -> Maybe Color
getColor note scales = fst <$> find (hasNote note . snd) (zip tangoColors scales)

drawFretboard :: Double -> Double -> Fretboard -> [Scale] -> C.Render ()
drawFretboard w h fb scales = do
  setColor bgColor
  C.rectangle 0 0 w h
  C.fill
  setColor fgColor

  C.setLineWidth (lineWidth * 10.0)
  mkLine $ Line topLeft (Point px (py + bh))
  C.setLineWidth lineWidth

  mkLines frets   (Point fretw 0) (Line (topLeft + Point fretw 0) (topLeft + Point fretw bh))
  mkLines strings (Point 0 freth) (Line topLeft (topLeft + Point bw 0))
  C.stroke

  drawInlays inlays
  drawFrets (tuning fb)
  where
    lineWidth = 1.0
    fgColor   = Color 0 0 0
    bgColor   = Color 1 1 1

    frets   = 23
    strings = stringCount fb

    inlays = [(3, 1), (5, 1), (7, 1), (9, 1), (12, 2), (15, 1), (17, 1), (19, 1), (21, 1)]

    topLeft@(Point px py) = Point 50 20

    bw = w - 2 * px
    bh = h - 2 * py

    fretw  = bw / fromIntegral frets
    freth  = bh / fromIntegral (strings - 1)
    radius = (bh / fromIntegral strings) / 5

    fretx, frety :: Int -> Double
    fretx 0 = px
    fretx i = px + fretw / 2.0 + realToFrac (i - 1) * fretw
    frety i = py + realToFrac i * freth

    fillCircle point = mkCircle radius point >> C.fill

    -- TODO: Support inlays with more than two dots per fret
    drawInlay :: (Int, Int) -> C.Render ()
    drawInlay (f, 1) = do
      let x = fretx f
      fillCircle (Point x (py + bh / 2.0))

    drawInlay (f, 2) = do
      let x = fretx f
      fillCircle (Point x (py + 3.0 * freth / 2.0))
      fillCircle (Point x (py + 7.0 * freth / 2.0))

    drawInlay _ = return ()

    drawInlays = mapM_ drawInlay

    drawFret i j n = case getColor n scales of
      Just c  -> setColor c >> fillCircle (Point (fretx i) (frety j))
      Nothing -> return ()

    drawFrets = goy 0
      where
        goy  _ []        = return ()
        goy !j (root:ys) = gox 0 >> goy (j+1) ys

          where
            gox !i | i <= frets = drawFret i j (root+i) >> gox (i+1)
                   | otherwise  = return ()
