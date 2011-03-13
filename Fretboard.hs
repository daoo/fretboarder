module Fretboard where

import Data.List
import Note
import INote
import Scale 
import Intervals

type Color = (Double, Double, Double)

data Fret         = Fret INote [Color]
  deriving (Show)
type GuitarString = [Fret]
type Fretboard    = [GuitarString]

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = [ createGuitarString $ 4 * 12 + 7 -- E4
         , createGuitarString $ 3 * 12 + 2 -- B3
         , createGuitarString $ 3 * 12 + 10 -- G3
         , createGuitarString $ 3 * 12 + 5 -- D3
         , createGuitarString $ 2 * 12 + 0 -- A2
         , createGuitarString $ 2 * 12 + 7 -- E2
  ]

createGuitarString :: INote -> GuitarString
createGuitarString n = zipWith Fret (iterate (+1) n) $ repeat []

markScale :: Color -> Scale -> Fretboard -> Fretboard
markScale _ [] fb     = fb
markScale c (s:ss) fb = markScale c ss $ map (markString c s) fb

markString :: Color -> INote -> GuitarString -> GuitarString
markString _ _ []                             = []
markString c e (f@(Fret n cs):fs) | e == n    = (Fret n (c:cs)) : fs
                                  | otherwise = f : markString c e fs

takeFrets :: Int -> Fretboard -> Fretboard
takeFrets i = map (take i)

