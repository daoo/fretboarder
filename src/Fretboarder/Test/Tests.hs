module Tests where

import Test.QuickCheck

import INote
import Note

fromToINote :: INote -> Bool
fromToINote i = i == (toINote $ fromINote i)

