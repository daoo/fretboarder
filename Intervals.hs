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

instance Ord Note where
  compare n1 n2 | d == 0 = EQ
                | d > 0  = LT
                | d < 0  = GT
    where
      d = n1 `halfStepDistance` n2

  (<) n1 n2 = compare n1 n2 == LT
  (>) n1 n2 = compare n1 n2 == GT

diatonicToSemitones :: DiatonicInterval -> Interval
diatonicToSemitones = toInteger . fromEnum

diatonicToIntervals _ []     = []
diatonicToIntervals i (a:as) = b : diatonicToIntervals (i + b) as
  where
    b = d - i
    d = diatonicToSemitones a

toneDistance :: Tone -> Tone -> Interval
toneDistance t1 t2 = toInteger $ (fromEnum t1) - (fromEnum t2)

accidentalDistance :: Accidental -> Accidental -> Interval
accidentalDistance a1 a2 = toInteger $ (fromEnum a1) - (fromEnum a2)

halfStepDistance :: Note -> Note -> Interval
halfStepDistance (Note t1 o1 a1) (Note t2 o2 a2) = toInteger ( (o1 - o2) * 12 ) + (t1 `toneDistance` t2) + (a1 `accidentalDistance` a2)

noteFreq :: Note -> Float
noteFreq n = 2 ^ ( (halfStepDistance n a4) `div` 12 ) * 440

-- TODO: This could been done without recursion
interval :: Interval -> Note -> Note
interval 1 n         = semiUp n
interval 2 n         = toneUp n
interval i n | i > 2 = interval (i - 2) $ toneUp n

