{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Fretboarder.Music.Semitone ( Semitone ) where

import Control.Applicative
import Test.QuickCheck

-- |Represents a musical note.
--
-- Efficient non-reduntant representation. The value 0 is C0 and then it
-- increments in semi tones.
newtype Semitone = Semitone { mkSemitone :: Int }
  deriving (Show, Eq, Enum, Ord, Num, Real, Integral)

instance Arbitrary Semitone where
  arbitrary = Semitone <$> choose (0, 120)

  shrink = map Semitone . shrink . mkSemitone
