{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Fretboarder.Parser
  ( parseNote
  , parseOctave
  , parseTone
  , parseAccidental
  , parseSPN
  , parseOffsets
  , parseScale
  , parseExpr
  , parseExprs
  ) where

import Control.Applicative
import Data.Attoparsec.Text hiding (D)
import Fretboarder.Drawing.Expr
import Fretboarder.Music.RootedScale
import Fretboarder.Music.SPN
import Fretboarder.Music.Scale
import Fretboarder.Music.Note
import Fretboarder.Music.Western

parseNote :: Parser Note
parseNote = Note <$> decimal

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

parseToneAccident :: Parser (Tone, Accidental)
parseToneAccident = (,) <$> parseTone <*> parseAccidental

parseSPN :: Parser SPN
parseSPN = do
  t <- parseTone
  o <- parseOctave
  a <- parseAccidental
  return $ mkSPN o t a

parseOffsets :: Parser Scale
parseOffsets =
  (harmonicMinor   <$ "harmonic minor") <|>
  (melodicMinor    <$ "melodic minor") <|>
  (pentatonicMinor <$ "minor pentatonic") <|>
  (pentatonicMajor <$ "major pentatonic") <|>
  (blues           <$ "blues") <|>
  (ionian          <$ "ionian") <|>
  (dorian          <$ "dorian") <|>
  (phrygian        <$ "phrygian") <|>
  (lydian          <$ "lydian") <|>
  (mixolydian      <$ "mixolydian") <|>
  (aeolian         <$ "aeolian") <|>
  (locrian         <$ "locrian") <|>
  (major           <$ "major") <|>
  (minor           <$ "minor")

parseScale :: Parser RootedScale
parseScale = do
  n <- parseSPN
  skipSpace
  s <- parseOffsets
  return $ RootedScale (toSemi n) s

parseExpr :: Parser Expr
parseExpr =
  (FullScale <$> parseScale) <|>
  ((OnePitch . toSemi) <$> parseSPN) <|>
  ((OneTone . toOffset . uncurry mkPitchClass) <$> parseToneAccident)

parseExprs :: Parser [Expr]
parseExprs = sepBy1 parseExpr (skipSpace >> char ',' >> skipSpace)
