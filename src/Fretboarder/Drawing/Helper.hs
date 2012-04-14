--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Drawing.Helper where

import Fretboarder.Drawing.Backend
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

render :: Backend a => Point -> Expr PScale -> a ()
render size expr = drawFretboard size $ markList marks fb
  where
    marks = zip tangoColors $ makeList $ makeScales expr

    fb = takeFrets 23 ebgdae

tangoColors :: [Color]
tangoColors = [ (0.988235294117647, 0.917647058823529, 0.309803921568627)
              , (0.937254901960784, 0.16078431372549, 0.16078431372549)
              , (0.988235294117647, 0.686274509803922, 0.243137254901961)
              , (0.67843137254902, 0.498039215686275, 0.658823529411765)
              , (0.447058823529412, 0.623529411764706, 0.811764705882353)
              , (0.541176470588235, 0.886274509803922, 0.203921568627451)
              , (0.913725490196078, 0.725490196078431, 0.431372549019608)
              , (0.533333333333333, 0.541176470588235, 0.52156862745098) ]

