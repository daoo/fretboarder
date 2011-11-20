--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Fretboarder.Guitar.Scale where

import Fretboarder.Guitar.Interval
import Fretboarder.Guitar.Note

data Scale = Scale INote [Offset]
  deriving (Show)

hasNote :: Scale -> INote -> Bool
hasNote (Scale base offsets) note = note' `elem` (0 : offsets)
  where
    note' = (note - base) `mod` 12

repeatScale :: Scale -> [INote]
repeatScale (Scale note offsets) = note : concatMap (\n -> map (+n) offsets) xs
  where
    xs = iterate (+12) note

