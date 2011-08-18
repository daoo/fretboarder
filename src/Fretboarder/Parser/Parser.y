{
module Fretboarder.Parser.Parser where

import Data.Char

import Fretboarder.Guitar.Note

import Fretboarder.Parser.String
import Fretboarder.Parser.Lexer
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

Tone : tone { fromTone $1 }

Accidental : accidental { accidentalFromChar $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
