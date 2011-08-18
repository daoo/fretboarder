{
module Parser where

import Data.Char
import Lexer
}

%name guitar
%tokentype { Token }
%error { parseError }

%token
  tone       { TTone $$       }
  accidental { TAccidental $$ }
%%

Note : Tone            { Note $1 Natural }
     | Tone Accidental { Note $1 $2      }

Tone : tone { Tone $1 }

Accidental : accidental { accidentalFromChar $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Scale = Scale 
  deriving Show

data Note = Note Tone Accidental
  deriving Show

data Tone = Tone Char
  deriving Show

data Accidental = Natural | Flat | Sharp
  deriving Show

accidentalFromChar :: Char -> Accidental
accidentalFromChar '#' = Sharp
accidentalFromChar 'b' = Flat
accidentalFromChar _   = Natural
}
