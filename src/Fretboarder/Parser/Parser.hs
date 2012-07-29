--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Parser.Parser
  ( PNote (..)
  , PScale (..)
  , parseExpr
  ) where

import Fretboarder.Guitar.Note
import Fretboarder.Parser.Expr
import Text.ParserCombinators.Parsec hiding (State, token, tokens)

data PNote = PNote Tone Accidental
  deriving Show

data PScale = PScale PNote String
  deriving Show

operator :: Parser (Expr a -> Expr a -> Expr a)
operator = choice
  [ char '*' >> return Join
  , char '+' >> return Different ]

note :: Parser PNote
note = choice $ map (\(str, n) -> oneOf str >> acc n)
  [ ("Aa", A)
  , ("Bb", B)
  , ("Cc", C)
  , ("Dd", D)
  , ("Ee", E)
  , ("Ff", F)
  , ("Gg", G)
  ]
  where
    acc :: Tone -> Parser PNote
    acc t = (char '#' >> return (PNote t Sharp))
        <|> (char 'b' >> return (PNote t Flat))
        <|> return (PNote t Natural)

scale :: Parser PScale
scale = do
  n <- note
  spaces
  s <- many1 (letter <|> space)
  return $ PScale n s

-- HACK: Proper binary expression parser
expr :: Parser (Expr PScale)
expr = do
  s <- scale
  f <- option Set $ do
    spaces
    op <- operator
    spaces
    e <- expr
    return $ op e . Set
  return $ f s

parseExpr :: String -> Either ParseError (Expr PScale)
parseExpr = parse expr "expr"
