module Note where

import Data.Char
import Data.Maybe

type Octave = Int

data Tone = C | D | E | F | G | A | B
  deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Accidental = Natural | Flat | Sharp
  deriving (Eq, Ord, Enum, Read, Show, Bounded)

data Note = Note Octave Tone Accidental
  deriving (Eq)

instance Show Note where
  show (Note o t a) = toneToChar t : show o ++ showAccidental a

a4 = Note 4 A Natural
b4 = Note 4 B Natural
c4 = Note 4 C Natural
a5 = Note 5 A Natural
b5 = Note 5 B Natural

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
showNote (Note o t a) = (toneToChar t : show o) ++ showAccidental a

-- FIXME: instance Read
noteFromString :: String -> Maybe Note
noteFromString [t, o] = f (digitToInt o) (charToTone t) Natural
  where 
    f :: Octave -> Maybe Tone -> Accidental -> Maybe Note
    f _ Nothing _  = Nothing
    f o (Just t) a = Just $ Note o t a
noteFromString _ = Nothing

-- Math --

fixNote :: Note -> Note
fixNote (Note o B Sharp) = Note (o + 1) C Natural
fixNote (Note o C Flat)  = Note (o - 1) B Natural
fixNote (Note o E Sharp) = Note o F Natural
fixNote (Note o F Flat)  = Note o E Natural
fixNote n                = n

semiUp :: Note -> Note
semiUp (Note o t Natural) = Note o t Sharp
semiUp (Note o t Flat)    = Note o t Natural
semiUp (Note o t Sharp)   = toneUp $ Note o t Natural

toneUp :: Note -> Note
toneUp (Note o C a) = Note o D a
toneUp (Note o D a) = Note o E a
toneUp (Note o E a) = semiUp $ Note o F a
toneUp (Note o F a) = Note o G a
toneUp (Note o G a) = Note o A a
toneUp (Note o A a) = Note o B a
toneUp (Note o B a) = semiUp $ Note (o + 1) C a

