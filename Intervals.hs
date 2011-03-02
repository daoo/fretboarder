module Intervals where

import Note

-- 1 = Semitone
-- 2 = Tone
-- 3 = Tone + Semitone
type Interval  = Integer
type Intervals = [Integer]

data DiatonicInterval = PerfectUnison | Minor2 | Major2 | Minor3 | Major3 | Perfect4 | Tritone | Perfect5 |
                        Minor6 | Major6 | Minor7 | Major7 | PerfectOctave
  deriving (Show, Eq, Enum)

diatonicToSemitones :: DiatonicInterval -> Interval
diatonicToSemitones = toInteger . fromEnum

asdf _ []     = []
asdf i (a:as) = b : asdf (i + b) as
  where
    b = d - i
    d = diatonicToSemitones a

toneDistance :: Tone -> Tone -> Interval
toneDistance t1 t2 = toInteger $ (fromEnum t1) - (fromEnum t2)

halfStepDistance :: Note -> Note -> Interval
halfStepDistance (Note o1 t1 a1) (Note o2 t2 a2) = toInteger ( (o1 - o2) * 12 ) + (t1 `toneDistance` t2)

noteFreq :: Note -> Float
noteFreq n = 2 ^ ( (halfStepDistance n a4) `div` 12 ) * 440

-- TODO: This could been done without recursion
interval :: Interval -> Note -> Note
interval 1 n = semiUp n
interval 2 n = toneUp n
interval i n | i > 2 = interval (i - 2) $ toneUp n

