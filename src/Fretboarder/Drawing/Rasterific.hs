{-# LANGUAGE BangPatterns, LambdaCase #-}
module Fretboarder.Drawing.Rasterific
  ( drawFretboard ) where

import Codec.Picture.Types
import Data.Array.IArray
import Fretboarder.Drawing.Expr
import Fretboarder.Fretboard
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Music.Theory.Note

tangoColors :: Array Int PixelRGB8
tangoColors = listArray (0, 7)
  [ PixelRGB8 239 41  41
  , PixelRGB8 138 226 52
  , PixelRGB8 114 159 207
  , PixelRGB8 252 234 79
  , PixelRGB8 252 175 62
  , PixelRGB8 173 127 168
  , PixelRGB8 233 185 110
  , PixelRGB8 136 138 133
  ]

mkLines :: Int -> Vector -> Line -> [Primitive]
mkLines !count !delta = go 0
  where
    go !i !acc | i < count = LinePrim acc : go (i+1) (transform (+delta) acc)
               | otherwise = []

drawFretboard :: Int -> Int -> Fretboard -> [Expr] -> Image PixelRGB8
drawFretboard w h fb exprs =
  renderDrawing w h bgColor $ withTexture (uniformTexture fgColor) $ do
    stroke 10 JoinRound cap $
      line topleft botleft

    mapM_ (stroke 1 JoinRound cap . (:[])) $
      mkLines frets (V2 fretw 0) (Line (topleft + V2 fretw 0) (botleft + V2 fretw 0))

    mapM_ (stroke 1 JoinRound cap . (:[])) $
      mkLines strings (V2 0 freth) (Line topleft topright)

    mapM_ (fill . mkInlays) inlays
    mapM_ fillColored $ mkStrings (tuning fb)
  where
    fgColor = PixelRGB8 0 0 0
    bgColor = PixelRGB8 255 255 255
    cap     = (CapStraight 0, CapStraight 0)

    frets, strings :: Int
    frets   = 23
    strings = stringCount fb

    inlays :: [(Int, Int)]
    inlays = [(3, 1), (5, 1), (7, 1), (9, 1), (12, 2), (15, 1), (17, 1), (19, 1), (21, 1)]

    marginx, marginy :: Float
    marginx = 20
    marginy = 20

    boardw, boardh :: Float
    boardw = realToFrac w - 2.0 * marginx
    boardh = realToFrac h - 2.0 * marginy

    topleft, topright, botleft :: Vector
    topleft  = V2 marginx            marginy
    topright = V2 (marginx + boardw) marginy
    botleft  = V2 marginx            (marginy + boardh)
    --botright = V2 (marginx + boardw) (marginy + boardh)

    fretw, freth :: Float
    fretw  = boardw / realToFrac frets
    freth  = boardh / realToFrac (strings - 1)
    radius = (boardh / realToFrac strings) / 5.0

    fretx1 = marginx
    fretx2 = marginx + fretw / 2.0
    frety1 = marginy

    fretx :: Int -> Float
    fretx 0 = marginx
    fretx i = marginx + fretw / 2.0 + realToFrac (i - 1) * fretw

    -- frety :: Int -> Float
    -- frety i = marginy + realToFrac i * freth

    -- TODO: Support inlays with more than two dots per fret
    mkInlays (f, 1) =
      circle (V2 (fretx f) (marginy + boardh / 2.0)) radius

    mkInlays (f, 2) =
      circle (V2 x (marginy + 3.0 * freth / 2.0)) radius ++
      circle (V2 x (marginy + 7.0 * freth / 2.0)) radius
      where
        x = fretx f

    mkInlays _ = []

    mkFret !x !y !n = case getColor n of
      []    -> []
      (c:_) -> [(c, circle (V2 x y) radius)]

    mkStrings = go 0 frety1
      where
        go :: Int -> Float -> [Note] -> [(PixelRGB8, [Primitive])]
        go  _  _ []        = []
        go !j !y (root:ys) = mkString y root ++ go (j+1) (y+freth) ys

    mkString !y !root = mkFret fretx1 y root ++ go 1 fretx2
      where
        go !i !x | i <= frets = mkFret x y (root +. Offset i) ++ go (succ i) (x+fretw)
                 | otherwise  = []

    fillColored (c, xs) = withTexture (uniformTexture c) $ fill xs

    getColor n = map (tangoColors !) $ semiIndex n exprs
