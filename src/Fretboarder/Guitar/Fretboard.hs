--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Fretboard where

import Data.List

import Fretboarder.Drawing.Color

import Fretboarder.Guitar.Note
import Fretboarder.Guitar.INote
import Fretboarder.Guitar.Scale 
import Fretboarder.Guitar.Intervals

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

markString :: Color -> INote -> GuitarString -> GuitarString
markString _ _ []                             = []
markString c e (f@(Fret n cs):fs) | e == n    = (Fret n (c:cs)) : fs
                                  | otherwise = f : markString c e fs

markScale :: Color -> Scale -> Fretboard -> Fretboard
markScale _ [] fb     = fb
markScale c (s:ss) fb = markScale c ss $ map (markString c s) fb

markFretboard :: [(Color, Scale)] -> Fretboard -> Fretboard
markFretboard [] fb           = fb
markFretboard ((c, is):ls) fb = markScale c is fb'
  where
    fb' = markFretboard ls fb

takeFrets :: Int -> Fretboard -> Fretboard
takeFrets i = map (take i)

