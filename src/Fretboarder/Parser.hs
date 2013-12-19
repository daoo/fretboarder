{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Fretboarder.Parser
  ( parseScale
  ) where

import Control.Applicative
import Data.Attoparsec.Text hiding (D)
import Fretboarder.Music.Note
import Fretboarder.Music.Offset
import Fretboarder.Music.Scale

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

parseNote :: Parser Note
parseNote = Note 0 <$> parseTone <*> parseAccidental

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
  n <- parseNote
  _ <- space
  s <- parseOffsets
  return $ Scale (noteToI n) s
