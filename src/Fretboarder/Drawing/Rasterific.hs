{-# LANGUAGE BangPatterns, LambdaCase #-}
module Fretboarder.Drawing.Rasterific (drawFretboard) where

import Codec.Picture.Types
import Fretboarder.Drawing.Expr
import Fretboarder.Fretboard
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Music.Theory.Note

mkLines :: Int -> Vector -> Line -> [Primitive]
mkLines !count !delta = go 0
  where
    go !i !acc | i < count = LinePrim acc : go (i+1) (transform (+delta) acc)
               | otherwise = []

drawFretboard :: Int -> Int -> Fretboard -> [Expr] -> Image PixelRGBA8
drawFretboard w h fb exprs =
  renderDrawing w h bgColor $ withTexture (uniformTexture fgColor) $ do
    strokeFat $ line topleft botleft

    mapM_ strokeThin $
      mkLines frets (V2 fretw 0) (Line (topleft + V2 fretw 0) (botleft + V2 fretw 0))

    mapM_ strokeThin $
      mkLines strings (V2 0 freth) (Line topleft topright)

    mapM_ (fill . mkInlays) inlays
    mapM_ fillColored $ mkStrings (tuning fb)
  where
    fgColor = PixelRGBA8 0 0 0 255
    bgColor = PixelRGBA8 255 255 255 255
    cap     = (CapStraight 0, CapStraight 0)

    strokeFat = stroke 10 JoinRound cap
    strokeThin = stroke 1 JoinRound cap . (:[])
    fillColored (c, xs) = withTexture (uniformTexture c) $ fill xs

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

    mkFret !x !y !n = case getColors n of
      []    -> []
      (c:_) -> [(c, circle (V2 x y) radius)]

    mkStrings = go 0 frety1
      where
        go :: Interval -> Float -> [Note] -> [(PixelRGBA8, [Primitive])]
        go  _  _ []        = []
        go !j !y (root:ys) = mkString y root ++ go (j+1) (y+freth) ys

    mkString !y !root = mkFret fretx1 y root ++ go 1 fretx2
      where
        go :: Interval -> Float -> [(PixelRGBA8, [Primitive])]
        go !i !x | fromIntegral i <= frets = mkFret x y (root .+ i) ++ go (succ i) (x+fretw)
                 | otherwise = []

    getColors :: Note -> [PixelRGBA8]
    getColors n = map getColor $ semiIndex n exprs

    getColor :: Int -> PixelRGBA8
    getColor = \case
      0 -> PixelRGBA8 239 41  41  255
      1 -> PixelRGBA8 138 226 52  255
      2 -> PixelRGBA8 114 159 207 255
      3 -> PixelRGBA8 252 234 79  255
      4 -> PixelRGBA8 252 175 62  255
      5 -> PixelRGBA8 173 127 168 255
      6 -> PixelRGBA8 233 185 110 255
      7 -> PixelRGBA8 136 138 133 255
      _ -> PixelRGBA8 255 0   0   255 -- error color
