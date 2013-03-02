--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Fretboard where

import Fretboarder.Drawing.Color
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale

data Fret = Fret INote [Color]
  deriving (Show)

type GuitarString = [Fret]
type Fretboard    = [GuitarString]

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = map (createGuitarString . toINote)
  [ Note E 4 Natural
  , Note B 3 Natural
  , Note G 3 Natural
  , Note D 3 Natural
  , Note A 2 Natural
  , Note E 2 Natural
  ]

createGuitarString :: INote -> GuitarString
createGuitarString n = zipWith Fret (iterate (+1) n) $ repeat []

takeFrets :: Int -> Fretboard -> Fretboard
takeFrets = map . take

markString :: Color -> Scale -> GuitarString -> GuitarString
markString _ _ []                                           = []
markString c scale (f@(Fret n cs) : fs) | scale `hasNote` n = Fret n (c:cs) : markString c scale fs
                                        | otherwise         = f : markString c scale fs

-- Note that the fretboard have to be finite
markFretboard :: Color -> Scale -> Fretboard -> Fretboard
markFretboard = (map .) . markString

markList :: [(Color, Scale)] -> Fretboard -> Fretboard
markList = flip (foldr (uncurry markFretboard))
