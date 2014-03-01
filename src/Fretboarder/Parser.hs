{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Fretboarder.Parser (parseNote, parseExpressions, parseSPN, parseExprs) where

import Control.Applicative
import Data.Attoparsec.Text hiding (D)
import Data.Text (pack)
import Fretboarder.Drawing.Expr
import Music.Theory.Note
import Music.Theory.SPN
import Music.Theory.Scale
import Music.Theory.Scale.Rooted
import Music.Theory.Scale.Western

parseNote :: Parser Note
parseNote = fromIntegral <$> (decimal :: Parser Int)

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
  (triadMinor      <$ "minor triad") <|>
  (triadMajor      <$ "major triad") <|>
  (triadDimnished  <$ "dimnished triad") <|>
  (triadAugmented  <$ "augmented triad") <|>
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

parseScale :: Parser Rooted
parseScale = do
  n <- parseSPN
  skipSpace
  s <- parseOffsets
  return $ Rooted (toNote n) s

parseExpr :: Parser Expr
parseExpr =
  (FullScale <$> parseScale) <|>
  ((OnePitch . toNote) <$> parseSPN) <|>
  ((OneTone . toOffset . uncurry mkPitchClass) <$> parseToneAccident)

parseExprs :: Parser [Expr]
parseExprs = sepBy1 parseExpr (skipSpace >> char ',' >> skipSpace)

parseExpressions :: String -> Either String [Expr]
parseExpressions = parseOnly parseExprs . pack
