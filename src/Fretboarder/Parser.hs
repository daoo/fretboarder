{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Fretboarder.Parser
  ( parseScale
  ) where

import Control.Applicative
import Data.Attoparsec.Text hiding (D)
import Fretboarder.Music.Note
import Fretboarder.Music.Offset
import Fretboarder.Music.SPN
import Fretboarder.Music.Scale

parseOctave :: Parser Octave
parseOctave = decimal

parseTone :: Parser Tone
parseTone =
  (A <$ char 'A') <|>
  (B <$ char 'B') <|>
  (C <$ char 'C') <|>
  (D <$ char 'D') <|>
  (E <$ char 'E') <|>
  (F <$ char 'F') <|>
  (G <$ char 'G')

parseAccidental :: Parser Accidental
parseAccidental =
  (Sharp <$ char '#') <|>
  (Flat <$ char 'b') <|>
  pure Natural

parseSPN :: Parser SPN
parseSPN = do
  t <- parseTone
  o <- parseOctave
  a <- parseAccidental
  return $ SPN o t a

parseOffsets :: Parser [Offset]
parseOffsets =
  (majorOffsets <$ "major") <|>
  (minorOffsets <$ "minor") <|>
  (harmonicMinor <$ "harmonic minor") <|>
  (melodicMinor <$ "melodic minor") <|>
  (minorPentatonic <$ "minor pentatonic") <|>
  (majorPentatonic <$ "major pentatonic") <|>
  (bluesOffsets <$ "blues") <|>
  (ionianMode <$ "ionian") <|>
  (dorianMode <$ "dorian") <|>
  (phrygianMode <$ "phrygian") <|>
  (lydianMode <$ "lydian") <|>
  (mixolydianMode <$ "mixolydian") <|>
  (aeolianMode <$ "aeolian") <|>
  (locrianMode <$ "locrian")

parseScale :: Parser Scale
parseScale = do
  n <- parseSPN
  _ <- space
  s <- parseOffsets
  return $ Scale (toSemi n) s
