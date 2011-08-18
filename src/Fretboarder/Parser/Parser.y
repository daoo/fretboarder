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

Parse : Note { Parse $1 }

Note : Tone            { PNote $1 Natural }
     | Tone Accidental { PNote $1 $2      }

Tone : tone { readTone $1 }

Accidental : accidental { readAccidental $1 }

{

data Parse = Parse PNote

data PNote = PNote Tone Accidental

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Parse
parse = guitar . lexer
}
