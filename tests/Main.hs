module Main where

import Fretboarder.Music.SPN
import Fretboarder.Music.Semitone
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (Assertion, (@?=))

prop_fromToSemi :: Semitone -> Bool
prop_fromToSemi i = i == toSemi (fromSemi i)

(@=) :: Eq a => a -> a -> Assertion
a @= b = a == b @?= True

(@/=) :: Eq a => a -> a -> Assertion
a @/= b = a == b @?= False

tests :: [Test]
tests =
  [ testProperty "Semitone conversion" prop_fromToSemi
  , testGroup "Semitone values"
    [ testCase "0 is C0"  (fromSemi 0 @= mkSPN 0 C Natural)
    , testCase "1 is C0#" (fromSemi 1 @= mkSPN 0 C Sharp)
    , testCase "2 is D0"  (fromSemi 2 @= mkSPN 0 D Natural)
    , testCase "2 is D0"  (fromSemi 2 @= mkSPN 0 D Natural)
    , testCase "48 is C4"  (fromSemi 48 @= mkSPN 4 C Natural)
    ]
  , testGroup "Note equalities"
    [ testCase "A4# = B4b" (mkSPN 4 A Sharp   @= mkSPN 4 B Flat)
    , testCase "B4  = C5b" (mkSPN 4 B Natural @= mkSPN 5 C Flat)
    , testCase "B4# = C5"  (mkSPN 4 B Sharp   @= mkSPN 5 C Natural)
    , testCase "E4  = F4b" (mkSPN 4 E Natural @= mkSPN 4 F Flat)
    , testCase "E4# = F4"  (mkSPN 4 E Sharp   @= mkSPN 4 F Natural)

    , testCase "B5# /= C5" (mkSPN 5 B Sharp @/= mkSPN 5 C Natural)
    ]
  ]

main :: IO ()
main = defaultMain tests
