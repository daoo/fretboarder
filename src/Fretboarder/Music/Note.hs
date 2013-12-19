module Fretboarder.Music.Note where

import Fretboarder.Music.Semitone

class Note n where
  fromSemi :: Semitone -> n
  toSemi   :: n -> Semitone
