module Fretboard where

import Note
import Scale 
import Intervals

type Color = (Int, Int, Int)

data Fret = Fret Note [Color]

data String    = String Note [Fret]
type Fretboard = [String]

-- The fretboard for a standard EBGDAE tuning
ebgdae :: Fretboard
ebgdae = [ createString (Note 6 E Natural)
         , createString (Note 

createString :: Note -> String
createString n = iterate semiUp n

markScale :: Color -> Scale -> Fretboard -> Fretboard
markScale c s = map (markString c s)

markString :: Color -> Note -> String -> String
markString c n (String nutNote frets) | n halfStepDistance (head s)

