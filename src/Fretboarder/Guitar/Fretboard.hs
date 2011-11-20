--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Fretboard where

import Fretboarder.Drawing.Color

import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale

data Fret = Fret INote [Color]
  deriving (Show)

addColor :: Color -> Fret -> Fret
addColor c (Fret n cs) = Fret n (c:cs)

type GuitarString = [Fret]
type Fretboard    = [GuitarString]

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = [ createGuitarString $ toINote $ Note E 4 Natural
         , createGuitarString $ toINote $ Note B 3 Natural
         , createGuitarString $ toINote $ Note G 3 Natural
         , createGuitarString $ toINote $ Note D 3 Natural
         , createGuitarString $ toINote $ Note A 2 Natural
         , createGuitarString $ toINote $ Note E 2 Natural
  ]

createGuitarString :: INote -> GuitarString
createGuitarString n = zipWith Fret (iterate (+1) n) $ repeat []

takeFrets :: Int -> Fretboard -> Fretboard
takeFrets i = map (take i)

markString :: Color -> Scale -> GuitarString -> GuitarString
markString _ _ []                    = []
markString c scale (f@(Fret n _):fs) = f' : markString c scale fs
  where
    f' = if scale `hasNote` n
           then addColor c f
           else f

-- Note that the fretboard have to be finite
markFretboard :: Color -> Scale -> Fretboard -> Fretboard
markFretboard c scale = map (markString c scale)

markList :: [(Color, Scale)] -> Fretboard -> Fretboard
markList lst fb = foldr (uncurry markFretboard) fb lst

