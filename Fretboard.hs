module Fretboard where

import Note
import Scale 
import Intervals

type Color = (Int, Int, Int)

data Fret         = Fret Note [Color]
  deriving (Show)
type GuitarString = (Note, [Fret])
type Fretboard    = [GuitarString]

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = [ createGuitarString (Note 4 E Natural)
         , createGuitarString (Note 3 B Natural)
         , createGuitarString (Note 3 G Natural)
         , createGuitarString (Note 3 D Natural)
         , createGuitarString (Note 2 A Natural)
         , createGuitarString (Note 2 E Natural)
  ]

createGuitarString :: Note -> GuitarString
createGuitarString n = (n, zipWith Fret notes $ repeat [])
  where
    notes = iterate semiUp n

markScale :: Color -> Scale -> Fretboard -> Fretboard
markScale _ [] fb     = fb
markScale c (s:ss) fb = markScale c ss $ markNote c s fb

markNote :: Color -> Note -> Fretboard -> Fretboard
markNote c n = map (markString c n)

markString :: Color -> Note -> GuitarString -> GuitarString
markString _ _ (nut, [])                            = (nut, [])
markString c e gs@(nut, (Fret n cs:ss)) | e == n    = (nut, (Fret n (c:cs)):ss)
                                        | otherwise = (nut, ns)
  where
    (_, ns) = markString c e (nut, ss)

takeFrets :: Int -> Fretboard -> Fretboard
takeFrets i = map (mapSnd $ take i)
  where
    mapSnd :: (a -> b) -> (c, a) -> (c, b)
    mapSnd f (a, b) = (a, f b)

