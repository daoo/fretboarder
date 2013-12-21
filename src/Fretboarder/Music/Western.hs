module Fretboarder.Music.Western
  ( chromatic
  , major
  , minor
  , harmonicMinor
  , melodicMinor
  , pentatonicMinor
  , pentatonicMajor
  , blues
  , ionian
  , dorian
  , phrygian
  , lydian
  , mixolydian
  , aeolian
  , locrian
  , triadMinor
  , triadMajor
  , triadDimnished
  , triadAugmented
  ) where

import Fretboarder.Music.Scale

chromatic :: Scale
chromatic = fromOffsets [0,1,2,3,4,5,6,7,8,9,10,11]

major, minor :: Scale
major = fromOffsets [0,2,4,5,7,9,11]
minor = fromOffsets [0,2,3,5,7,8,10]

harmonicMinor, melodicMinor :: Scale
harmonicMinor = raise 7 minor
melodicMinor  = lower 3 major

pentatonicMinor, pentatonicMajor :: Scale
pentatonicMinor = fromOffsets [0,3,5,7,10]
pentatonicMajor = fromOffsets [0,2,4,7,9]

blues :: Scale
blues = fromOffsets [0,6]

ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: Scale
ionian     = major
dorian     = raise 6 minor
phrygian   = lower 2 minor
lydian     = raise 4 major
mixolydian = lower 7 major
aeolian    = minor
locrian    = lower 2 $ lower 5 minor

triadMinor, triadMajor, triadDimnished, triadAugmented :: Scale
triadMinor = fromOffsets [0,3,7]
triadMajor = fromOffsets [0,4,7]
triadDimnished = fromOffsets [0,3,6]
triadAugmented = fromOffsets [0,4,8]
