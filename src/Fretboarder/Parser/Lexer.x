{
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

{
data Token = TTone Char
           | TAccidental Char
           | TScale String
 deriving (Eq,Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}

