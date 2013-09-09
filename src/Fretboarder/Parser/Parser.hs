{-# LANGUAGE LambdaCase #-}
module Fretboarder.Parser.Parser
  ( parseExprScale
  ) where

import Control.Applicative ((<$>))
import Data.Char
import Fretboarder.Guitar.Interval
import Fretboarder.Guitar.Note
import Fretboarder.Guitar.Scale
import Fretboarder.Parser.Expr
import Text.Parsec
import Text.Parsec.String

data PNote = PNote Tone Accidental
  deriving Show

data PScale = PScale PNote String
  deriving Show

toneToChar :: Tone -> Char
toneToChar = \case
  A -> 'A'
  B -> 'B'
  C -> 'C'
  D -> 'D'
  E -> 'E'
  F -> 'F'
  G -> 'G'

parsePNote :: Parser PNote
parsePNote = f A <|> f B <|> f C <|> f D <|> f E <|> f F <|> f G
  where
    f t = (char c <|> char (toLower c)) >> (PNote t <$> acc)
      where c = toneToChar t

    acc = (char '#' >> return Sharp)
      <|> (char 'b' >> return Flat)
      <|> return Natural

parsePScale :: Parser PScale
parsePScale = do
  n <- parsePNote
  spaces
  s <- many (letter <|> space)
  return $ PScale n s

parseOperator :: Parser (Expr a -> Expr a -> Expr a)
parseOperator = (char '*' >> return Join)
            <|> (char '+' >> return Different)

-- HACK: Proper binary expression parser
parseExpr :: Parser a -> Parser (Expr a)
parseExpr p = do
  s <- p
  f <- option Set $ do
    spaces
    op <- parseOperator
    spaces
    e <- parseExpr p
    return $ op e . Set
  return $ f s

parseExprScale :: String -> Either ParseError (Expr Scale)
parseExprScale = parse (makeScales <$> parseExpr parsePScale) ""

levenshtein :: String -> String -> Int
levenshtein s t = d !! length s !! length t
  where
    d = [ [ distance m n | n <- [0..length t] ] | m <- [0..length s] ]

    distance i 0 = i
    distance 0 j = j
    distance i j = minimum [a, b, c]
      where
        a = d !! (i - 1) !! j + 1
        b = d !! i !! (j - 1) + 1
        c = d !! (i - 1) !! (j - 1) + f (s !! (i - 1) == t !! (j - 1))
          where
            f True  = 0
            f False = 1

readOffsets :: String -> [Offset]
readOffsets str = snd $ minimum $ map f offsets
  where
    f :: (String, [Offset]) -> (Int, [Offset])
    f (s, o) = (levenshtein s str, o)

    offsets :: [(String, [Offset])]
    offsets = [ ("major", majorScale)
              , ("minor", minorScale)

              , ("harmonic minor", harmonicMinor)
              , ("melodic minor", melodicMinor)

              , ("minor pentatonic", minorPentatonic)
              , ("major pentatonic", majorPentatonic)

              , ("blues", bluesScale)

              , ("ionian", ionianMode)
              , ("dorian", dorianMode)
              , ("phrygian", phrygianMode)
              , ("lydian", lydianMode)
              , ("mixolydian", mixolydianMode)
              , ("aeolian", aeolianMode)
              , ("locrian", locrianMode) ]

makeScales :: Expr PScale -> Expr Scale
makeScales = fmap $ \(PScale (PNote tone accidental) scale) -> Scale (toINote (Note tone 1 accidental)) $ readOffsets scale
