{
--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Parser.Parser where

import Data.Char

import Fretboarder.Guitar.Note
import Fretboarder.Parser.Expr
import Fretboarder.Parser.Lexer
import Fretboarder.Parser.String
}

%name guitar
%tokentype { Token }
%error { parseError }

%token
  tone       { TTone $$       }
  accidental { TAccidental $$ }
  scale      { TScale $$      }
  '+'        { TPlus          }
  '*'        { TMult          }

%left '+'
%left '*'

%%

Parse ::                { Expr PScale       }
      : PScale          { Set $1            }
      | Parse '*' Parse { $1 `Join` $3      }
      | Parse '+' Parse { $1 `Different` $3 }

PScale ::            { PScale       }
       : PNote scale { PScale $1 $2 }

PNote ::                { PNote                                   }
      : tone            { PNote (readTone $1) Natural             }
      | tone accidental { PNote (readTone $1) (readAccidental $2) }

{
data PNote = PNote Tone Accidental
  deriving Show

data PScale = PScale PNote String
  deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Expr PScale
parse = guitar . lexer
}

-- vim: ft=happy :
