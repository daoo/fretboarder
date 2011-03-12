module Fretboard where

import Data.List
import Note
import Scale 
import Intervals

type Color = (Double, Double, Double)

data Fret         = Fret Note [Color]
  deriving (Show)
type GuitarString = [Fret]
type Fretboard    = [GuitarString]

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = [ createGuitarString (Note E 4 Natural)
         , createGuitarString (Note B 3 Natural)
         , createGuitarString (Note G 3 Natural)
         , createGuitarString (Note D 3 Natural)
         , createGuitarString (Note A 2 Natural)
         , createGuitarString (Note E 2 Natural)
  ]

createGuitarString :: Note -> GuitarString
createGuitarString n = zipWith Fret (iterate semiUp n) $ repeat []

markScale :: Color -> Scale -> Fretboard -> Fretboard
markScale _ [] fb     = fb
markScale c (s:ss) fb = markScale c ss $ map (markString c s) fb

markString :: Color -> Note -> GuitarString -> GuitarString
markString _ _ []                             = []
markString c e (f@(Fret n cs):fs) | e == n    = (Fret n (c:cs)) : fs
                                  | otherwise = f : markString c e fs

takeFrets :: Int -> Fretboard -> Fretboard
takeFrets i = map (take i)

showGuitarString :: GuitarString -> String
showGuitarString frets = concat $ intersperse " " $ map (\ (Fret n c) -> show n ++ " " ++ show c ) frets

--showFretboard :: Fretboard -> String
showFretboard fb = concat $ intersperse "\n" $ map showGuitarString fb
