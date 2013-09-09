module Fretboarder.Drawing.Cairo
  ( setColor
  , setLineWidth
  , strokeRectangle
  , fillRectangle
  , strokeLines
  , fillCircle
  , fillArcs
  , line
  ) where

import Fretboarder.Drawing.Backend
import qualified Graphics.Rendering.Cairo as C

instance Backend C.Render where
  setColor (r, g, b) = C.setSourceRGB r g b
  setLineWidth       = C.setLineWidth

  strokeRectangle (x, y) (w, h) = C.rectangle x y w h >> C.stroke
  fillRectangle (x, y) (w, h)   = C.rectangle x y w h >> C.fill

  strokeLines ls = mapM_ line ls >> C.stroke

  fillCircle r (x, y)  = C.arc x y r 0 pi2 >> C.fill
  fillArcs r (x, y) cs = mapM_ f cs >> C.fill
    where
      f (c, (a, b)) = setColor c >> C.arc x y r a b

line :: Line -> C.Render ()
line ((x1, y1), (x2, y2)) = C.moveTo x1 y1 >> C.lineTo x2 y2
