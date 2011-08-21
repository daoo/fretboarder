--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.INote where

-- An internal note represntation where natural A in octave 0 represents the INote
-- number 0. A# in octave 0: INote 1, and so forth.
type INote = Integer

