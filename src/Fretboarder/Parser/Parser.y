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
%monad { E } { thenE } { returnE }

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
data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
thenE m k = case m of 
  Ok a     -> k a
  Failed e -> Failed e

returnE :: a -> E a
returnE = Ok

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
  Ok a     -> Ok a
  Failed e -> k e

data PNote = PNote Tone Accidental
  deriving Show

data PScale = PScale PNote String
  deriving Show

happyError :: [Token] -> E a
happyError _ = failE "Parse error"

parse :: String -> E (Expr PScale)
parse = guitar . lexer
}

-- vim: ft=happy :
