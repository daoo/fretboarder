--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Fretboard where

import Fretboarder.Drawing.Color

import Fretboarder.Guitar.Note

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

-- [a] is an ascending list of sorted unique integers
elem2 :: INote -> [INote] -> (Bool, [INote])
elem2 _ []                       = (False, [])
elem2 e notes@(x:xs) | e < x     = (False, notes)
                     | e == x    = (True, xs)
                     | otherwise = elem2 e xs

markFret :: Color -> Fret -> [INote] -> (Fret, [INote])
markFret c f@(Fret a _) notes = case elem2 a notes of
                                  (True, xs)  -> (addColor c f, xs)
                                  (False, xs) -> (f, xs)

markString :: Color -> [INote] -> GuitarString -> GuitarString
markString _ [] gs        = gs
markString _ _ []         = []
markString c notes (f:fs) = f' : markString c notes' fs
  where
    (f', notes') = markFret c f notes

-- Note that the fretboard have to be finite while the list of INotes doesn't
markFretboard :: Color -> [INote] -> Fretboard -> Fretboard
markFretboard c notes = map (markString c notes)

markList :: [(Color, [INote])] -> Fretboard -> Fretboard
markList lst fb = foldr (uncurry markFretboard) fb lst
