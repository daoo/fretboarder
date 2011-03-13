module Note where

import Data.Char
import Data.Maybe

import INote

type Octave = Int

data Tone = A | B | C | D | E | F | G
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

toINote :: Note -> INote
toINote (Note t o a) = 12 * o' + t' + a'
  where
    t' = case t of
      A -> 0
      B -> 2
      C -> 3
      D -> 5
      E -> 7
      F -> 8
      G -> 10
    o' = fromIntegral $ fromEnum o
    a' = case a of
      Flat    -> -1
      Natural -> 0
      Sharp   -> 1

fromINote :: INote -> Note
fromINote i = Note t o a
  where
    o      = fromIntegral $ i `div` 12
    (t, a) = case i `mod` 12 of
      0 -> (A, Natural)
      1 -> (A, Sharp)
      2 -> (B, Natural)
      3 -> (C, Natural)
      4 -> (C, Sharp)
      5 -> (D, Natural)
      6 -> (D, Sharp)
      7 -> (E, Natural)
      8 -> (F, Natural)
      9 -> (F, Sharp)
      10 -> (G, Natural)
      11 -> (G, Sharp)

fixNote :: Note -> Note
fixNote (Note B o Sharp) = Note C (o + 1) Natural
fixNote (Note C o Flat)  = Note B (o - 1) Natural
fixNote (Note E o Sharp) = Note F o Natural
fixNote (Note F o Flat)  = Note E o Natural
fixNote n                = n
