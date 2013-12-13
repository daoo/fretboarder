{-# LANGUAGE BangPatterns #-}
module Fretboarder.Drawing.Backend
  ( Color
  , Point
  , Size
  , Line

  , pi2

  , Backend
  , setColor
  , setLineWidth
  , strokeLines
  , strokeRectangle
  , fillRectangle
  , fillCircle
  , fillArcs

  , deltaLines
  , evenPie

  , Settings(..)
  , defaultSettings

  , drawFretboard
  ) where

import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Scale
import Fretboarder.Utility

type Color = (Double, Double, Double)
type Point = (Double, Double)
type Size  = (Double, Double)
type Line  = (Point, Point)

(+++) :: Point -> Point -> Point
(x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)

pi2 :: Double
pi2 = pi * 2.0

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
    angles = [0, pi2 / fromIntegral (length colors) .. ]

data Settings = Settings
  { getInlays :: [(Int, Int)]
  , getFretCount :: Int
  , getLineWidth :: Double
  , getFgColor :: Color
  , getBgColor :: Color
  } deriving Show

defaultSettings :: Settings
defaultSettings = Settings
  { getInlays    = [(3, 1), (5, 1), (7, 1), (9, 1), (12, 2), (15, 1), (17, 1), (19, 1), (21, 1)]
  , getLineWidth = 1.0
  , getFretCount = 23
  , getFgColor   = (0, 0, 0)
  , getBgColor   = (255, 255, 255)
  }

drawFretboard :: Backend a => Settings -> Size -> Fretboard -> Scale -> a ()
drawFretboard set (w, h) fb scale = do
  setColor $ getBgColor set
  fillRectangle (0, 0) (w, h)
  setColor $ getFgColor set

  setLineWidth $ getLineWidth set * 10.0
  strokeLines [(topLeft, (px, py + bh))]
  setLineWidth $ getLineWidth set

  deltaLines fretcount (fretw, 0) ((px + fretw, py), (px + fretw, py + bh))
  deltaLines stringcount (0, freth) (topLeft, (px + bw, py))

  mapM_ (fillCircle radius) $ inlays $ getInlays set
  fillPoints
  where
    topLeft@(px, py) = (50, 20)

    bw = w - 2 * px
    bh = h - 2 * py

    fretcount   = getFretCount set
    stringcount = stringCount fb
    fretw       = bw / fromIntegral fretcount
    freth       = bh / fromIntegral (stringcount - 1)
    radius      = (bh / fromIntegral stringcount) / 5

    fretx, frety :: Int -> Double
    fretx 0 = px
    fretx i = px + fretw / 2.0 + fromIntegral (i - 1) * fretw
    frety i = py + fromIntegral i * freth

    -- TODO: Support inlays with more than two dots per fret
    inlay 1 x = [(x, py + bh / 2.0)]
    inlay 2 x = [(x, py + 3.0 * freth / 2.0), (x, py + 7.0 * freth / 2.0)]
    inlay _ _ = []

    inlays :: [(Int, Int)] -> [Point]
    inlays []            = []
    inlays ((f, c) : as) = inlay c (fretx f) ++ inlays as

    fillPoints = goy 0
      where
        goy !j | j < stringcount = gox 0 >> goy (j+1)
               | otherwise       = return ()

          where
            gox !i | i < fretcount = fillCircle radius (fretx i, frety j) >> gox (i+1)
                   | otherwise     = return ()
