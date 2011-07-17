module Parser where

import Data.Char
import Note

instance Show Note where
  show (Note t o a) = toneToChar t : show o ++ showAccidental a

parse :: String -> String
parse s = undefined

parseNote :: String -> Maybe String
parseNote ('A':'#':s) = undefined

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


