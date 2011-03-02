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
markScale _ [] b     = b
markScale c (s:ss) b = markScale c ss $ map (markNote c s) b

markNote :: Color -> Note -> GuitarString -> GuitarString
markNote c n s@(nutNote, frets) | d >= 0    = markAt c d s
                                | otherwise = s
  where
    d = n `halfStepDistance` nutNote

markAt :: Color -> Integer -> GuitarString -> GuitarString
markAt c 0 gs@(nut, (Fret n cs):ss) = (nut, (Fret n (c:cs):ss))
markAt c i gs@(nut, s:ss)           = (nut, ns)
  where
    (_, ns) = markAt c (i - 1) gs


takeFrets :: Int -> Fretboard -> Fretboard
takeFrets i = map (mapSnd $ take i)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)
