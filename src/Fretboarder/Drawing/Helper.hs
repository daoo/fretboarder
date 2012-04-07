--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Drawing.Helper where

import Graphics.Rendering.Cairo hiding (scale)

import Fretboarder.Drawing.Cairo
import Fretboarder.Drawing.CairoExt
import Fretboarder.Drawing.Color
import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale
import Fretboarder.Parser.Expr
import Fretboarder.Parser.Parser
import Fretboarder.Parser.String

makeScales :: Expr PScale -> Expr Scale
makeScales = fmap f
  where
    f (PScale (PNote tone accidental) scale) = Scale (toINote (Note tone 1 accidental)) $ readOffsets scale

makeList :: Expr Scale -> [Scale]
makeList (Set scale)              = [scale]
makeList (Different e1 e2)        = makeList e1 ++ makeList e2
makeList (Join (Set s1) (Set s2)) = [s1 `joinScales` s2]
makeList _                        = error "Can not transform Expr to list"

render :: Point -> Expr PScale -> Render ()
render size expr = drawFretboard size $ markList marks fb
  where
    marks = zip tangoColors $ makeList $ makeScales expr

    fb = takeFrets 23 ebgdae

