{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.Semitone ( Semitone ) where

import Control.Applicative
import Test.QuickCheck

-- |Represents a musical note.
--
-- Efficient non-reduntant representation. These values should be treated as
-- the MIDI note numbers, that is zero is the natural C in octave -1. Each
-- integer increment represents a semitone increment.
newtype Semitone = Semitone { mkSemitone :: Int }
  deriving (Show, Eq, Enum, Ord, Num, Real, Integral)

instance Arbitrary Semitone where
  arbitrary = Semitone <$> choose (0, 120)

  shrink = map Semitone . shrink . mkSemitone
