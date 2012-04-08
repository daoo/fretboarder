--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Drawing.Cairo where

import Graphics.Rendering.Cairo
import Fretboarder.Drawing.Backend

instance Backend Render where
  setColor (r, g, b) = setSourceRGB r g b
  setLineWidth = Graphics.Rendering.Cairo.setLineWidth

  strokeRectangle (x, y) (w, h) = rectangle x y w h >> stroke
  fillRectangle (x, y) (w, h) = rectangle x y w h >> fill

  strokeLines ls = mapM_ line ls >> stroke

  fillCircle r (x, y) = arc x y r 0 pi2 >> fill
  fillArcs r (x, y) = mapM_ f
    where
      f (c, (a, b)) = setColor c >> arc x y r a b

line :: Line -> Render ()
line ((x1, y1), (x2, y2)) = moveTo x1 y1 >> lineTo x2 y2

