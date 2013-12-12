{-# LANGUAGE LambdaCase #-}
module Fretboarder.Drawing.Helper where

import Fretboarder.Drawing.Backend
import Fretboarder.Guitar.Fretboard
import Fretboarder.Guitar.Scale
import Fretboarder.Parser.Expr

makeList :: Expr Scale -> [Scale]
makeList = \case
  Set scale              -> [scale]
  Different e1 e2        -> makeList e1 ++ makeList e2
  Join (Set s1) (Set s2) -> [s1 `joinScales` s2]
  _                      -> undefined

render :: Backend a => Settings -> Size -> Expr Scale -> a ()
render set size expr = drawFretboard set size $ markList marks fb
  where
    marks = zip tangoColors $ makeList expr

    fb = takeFrets 23 ebgdae

tangoColors :: [Color]
tangoColors = [ (0.988235294117647, 0.917647058823529, 0.309803921568627)
              , (0.937254901960784, 0.16078431372549, 0.16078431372549)
              , (0.988235294117647, 0.686274509803922, 0.243137254901961)
              , (0.67843137254902, 0.498039215686275, 0.658823529411765)
              , (0.447058823529412, 0.623529411764706, 0.811764705882353)
              , (0.541176470588235, 0.886274509803922, 0.203921568627451)
              , (0.913725490196078, 0.725490196078431, 0.431372549019608)
              , (0.533333333333333, 0.541176470588235, 0.52156862745098)
              ]
