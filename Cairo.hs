module Cairo where

import Data.Ratio
import Graphics.Rendering.Cairo

import Fretboard

drawFretboard :: (Double, Double) -> Fretboard -> Render ()
drawFretboard (w, h) fb = do
  newPath
  moveTo px py
  lineTo (w - px) py
  lineTo (w - px) (h - py)
  lineTo px (h - py)
  closePath

  stroke

  return ()

  where
    (px, py)         = (w - boardw, h - boardh)
    (boardw, boardh) = (w * 0.95, h * 0.95)
    (fretw, freth)   = (boardw / fretcount, boardh / stringcount)

    fretcount   = realToFrac $ length $ snd $ head fb
    stringcount = realToFrac $ length fb

test :: IO ()
test = do
  withSVGSurface "test.svg" 1280 1024 (\ s -> renderWith s $ drawFretboard (1280.0, 1024.0) (takeFrets 20 ebgdae))
