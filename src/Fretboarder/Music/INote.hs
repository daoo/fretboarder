{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.INote ( INote ) where

import Control.Applicative
import Test.QuickCheck

-- |Represents a musical note.
--
-- Efficient non-reduntant representation. Note number zero represents A in
-- octave 0. Increments in semitones.
newtype INote = INote { mkINote :: Int }
  deriving (Show, Eq, Enum, Ord, Num, Real, Integral)

instance Arbitrary INote where
  arbitrary = INote <$> choose (0, 120)

  shrink = map INote . shrink . mkINote
