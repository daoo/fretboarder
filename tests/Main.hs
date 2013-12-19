module Main where

import Fretboarder.Guitar.SPN
import Fretboarder.Guitar.Semitone
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (Assertion, (@?=))

prop_fromToSemitone :: Semitone -> Bool
prop_fromToSemitone i = i == noteToI (iToNote i)

(@=) :: Eq a => a -> a -> Assertion
a @= b = a == b @?= True

tests :: [Test]
tests =
  [ testProperty "Semitone <-> Note conversion" prop_fromToINote
  , testGroup "Note equalities"
    [ testCase "A4# = B4b" (Note 4 A Sharp   @= Note 4 B Flat)
    , testCase "B4  = C5b" (Note 4 B Natural @= Note 5 C Flat)
    , testCase "B4# = C5"  (Note 4 B Sharp   @= Note 5 C Natural)
    , testCase "E4  = F4b" (Note 4 E Natural @= Note 4 F Flat)
    , testCase "E4# = F4"  (Note 4 E Sharp   @= Note 4 F Natural)
    ]
  ]

main :: IO ()
main = defaultMain tests
