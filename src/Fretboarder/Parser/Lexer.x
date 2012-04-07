{
--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Parser.Lexer where
}

%wrapper "basic"

$alpha      = [a-zA-Z]
$tone       = [ABCDEFG]
$accidental = [b\#]

tokens :-

  $white+     ;
  $tone       { \s -> TTone (head s)       }
  $accidental { \s -> TAccidental (last s) }
  $alpha{2,}  { \s -> TScale s             }
  "+"         { \_ -> TPlus                }
  "*"         { \_ -> TMult                }
{
data Token = TTone Char
           | TAccidental Char
           | TScale String
           | TPlus | TMult
 deriving (Eq,Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}

-- vim: ft=alex :
