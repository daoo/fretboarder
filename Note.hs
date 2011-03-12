module Note where

import Data.Char
import Data.Maybe

type Octave = Int

data Tone = C | D | E | F | G | A | B
  deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Accidental = Natural | Flat | Sharp
  deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Note = Note Tone Octave Accidental
  deriving (Eq)

instance Show Note where
  show (Note t o a) = toneToChar t : show o ++ showAccidental a

a4 = Note A 4 Natural
b4 = Note B 4 Natural
c4 = Note C 4 Natural
a5 = Note A 5 Natural
b5 = Note B 5 Natural

-- To/From Strings --

toneToChar :: Tone -> Char
toneToChar C = 'C'
toneToChar D = 'D'
toneToChar E = 'E'
toneToChar F = 'F'
toneToChar G = 'G'
toneToChar A = 'A'
toneToChar B = 'B'

charToAccidental :: Char -> Accidental
charToAccidental '#' = Sharp
charToAccidental 'b' = Flat
charToAccidental _   = Natural

charToTone :: Char -> Maybe Tone
charToTone 'C' = Just C
charToTone 'D' = Just D
charToTone 'E' = Just E
charToTone 'F' = Just F
charToTone 'G' = Just G
charToTone 'A' = Just A
charToTone 'B' = Just B
charToTone  _  = Nothing

showAccidental :: Accidental -> String
showAccidental Natural = []
showAccidental Flat    = "b"
showAccidental Sharp   = "#"

showNote :: Note -> String
showNote (Note t o a) = (toneToChar t : show o) ++ showAccidental a

-- FIXME: instance Read
noteFromString :: String -> Maybe Note
noteFromString [t, o] = f (charToTone t) (digitToInt o) Natural
  where 
    f :: Maybe Tone -> Octave -> Accidental -> Maybe Note
    f Nothing _ _  = Nothing
    f (Just t) o a = Just $ Note t o a
noteFromString _ = Nothing

-- Math --

fixNote :: Note -> Note
fixNote (Note B o Sharp) = Note C (o + 1) Natural
fixNote (Note C o Flat)  = Note B (o - 1) Natural
fixNote (Note E o Sharp) = Note F o Natural
fixNote (Note F o Flat)  = Note E o Natural
fixNote n                = n

semiUp :: Note -> Note
semiUp (Note o t Natural) = Note o t Sharp
semiUp (Note o t Flat)    = Note o t Natural
semiUp (Note o t Sharp)   = toneUp $ Note o t Natural

toneUp :: Note -> Note
toneUp (Note C o a) = Note D o a
toneUp (Note D o a) = Note E o a
toneUp (Note E o a) = semiUp $ Note F o a
toneUp (Note F o a) = Note G o a
toneUp (Note G o a) = Note A o a
toneUp (Note A o a) = Note B o a
toneUp (Note B o a) = semiUp $ Note C (o + 1) a

