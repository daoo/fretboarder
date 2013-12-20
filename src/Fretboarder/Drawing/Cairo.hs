{-# LANGUAGE BangPatterns, LambdaCase #-}
module Fretboarder.Drawing.Cairo
  ( drawFretboard ) where

import Data.Array.IArray
import Fretboarder.Drawing.Expr
import Fretboarder.Music.Fretboard
import Fretboarder.Music.Semitone
import qualified Graphics.Rendering.Cairo as C

data Color = Color {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data Line  = Line {-# UNPACK #-} !Point {-# UNPACK #-} !Point

(.+) :: Point -> Point -> Point
Point x1 y1 .+ Point x2 y2 = Point (x1+x2) (y1+y2)

tangoColors :: Array Int Color
tangoColors = listArray (0, 7)
  [ Color 0.937254901960784 0.160784313725490 0.160784313725490
  , Color 0.541176470588235 0.886274509803922 0.203921568627451
  , Color 0.447058823529412 0.623529411764706 0.811764705882353
  , Color 0.988235294117647 0.917647058823529 0.309803921568627
  , Color 0.988235294117647 0.686274509803922 0.243137254901961
  , Color 0.678431372549020 0.498039215686275 0.658823529411765
  , Color 0.913725490196078 0.725490196078431 0.431372549019608
  , Color 0.533333333333333 0.541176470588235 0.521568627450980
  ]

(.+.) :: Line -> Point -> Line
Line a b .+. p = Line (a .+ p) (b .+ p)

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
    go !i !acc | i < count = mkLine acc >> go (i+1) (acc .+. delta)
               | otherwise = return ()

getColor :: Semitone -> [Expr] -> [Color]
getColor n = map (tangoColors !) . semiIndex n

drawFretboard :: Double -> Double -> Fretboard -> [Expr] -> C.Render ()
drawFretboard w h fb e = do
  setColor bgColor
  C.rectangle 0 0 w h
  C.fill
  setColor fgColor

  C.setLineWidth (lineWidth * 10.0)
  mkLine $ Line topleft botleft
  C.setLineWidth lineWidth

  mkLines frets   (Point fretw 0) (Line (topleft .+ Point fretw 0) (botleft .+ Point fretw 0))
  mkLines strings (Point 0 freth) (Line topleft topright)
  C.stroke

  drawInlays inlays
  drawStrings (tuning fb)
  where
    lineWidth = 1.0
    fgColor   = Color 0 0 0
    bgColor   = Color 1 1 1

    frets, strings :: Int
    frets   = 23
    strings = stringCount fb

    inlays = [(3, 1), (5, 1), (7, 1), (9, 1), (12, 2), (15, 1), (17, 1), (19, 1), (21, 1)]

    marginx = 20
    marginy = 20

    boardw = w - 2 * marginx
    boardh = h - 2 * marginy

    topleft  = Point marginx            marginy
    topright = Point (marginx + boardw) marginy
    botleft  = Point marginx            (marginy + boardh)
    --botright = Point (marginx + boardw) (marginy + boardh)

    fretw  = boardw / realToFrac frets
    freth  = boardh / realToFrac (strings - 1)
    radius = (boardh / realToFrac strings) / 5.0

    fretx1 = marginx
    fretx2 = marginx + fretw / 2.0
    frety1 = marginy

    fretx :: Int -> Double
    fretx 0 = marginx
    fretx i = marginx + fretw / 2.0 + realToFrac (i - 1) * fretw

    -- frety :: Int -> Double
    -- frety i = marginy + realToFrac i * freth

    fillCircle point = mkCircle radius point >> C.fill

    -- TODO: Support inlays with more than two dots per fret
    drawInlay :: (Int, Int) -> C.Render ()
    drawInlay (f, 1) = do
      let x = fretx f
      fillCircle (Point x (marginy + boardh / 2.0))

    drawInlay (f, 2) = do
      let x = fretx f
      fillCircle (Point x (marginy + 3.0 * freth / 2.0))
      fillCircle (Point x (marginy + 7.0 * freth / 2.0))

    drawInlay _ = return ()

    drawInlays = mapM_ drawInlay

    drawFret !x !y !n = case getColor n e of
      []    -> return ()
      (c:_) -> setColor c >> fillCircle (Point x y)

    drawStrings :: [Semitone] -> C.Render ()
    drawStrings = go 0 frety1
      where
        go :: Int -> Double -> [Semitone] -> C.Render ()
        go  _  _ []        = return ()
        go !j !y (root:ys) = drawString y root >> go (j+1) (y+freth) ys

    drawString :: Double -> Semitone -> C.Render ()
    drawString !y !root = drawFret fretx1 y root >> go 1 fretx2
      where
        go :: Int -> Double -> C.Render ()
        go !i !x | i <= frets = drawFret x y (root + fromIntegral i) >> go (succ i) (x+fretw)
                 | otherwise  = return ()
