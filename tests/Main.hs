module Main where

import Fretboarder.Music.Note
import Fretboarder.Music.RootedScale
import Fretboarder.Music.SPN
import Fretboarder.Music.Scale
import Fretboarder.Music.Western
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck

genRemoveRandom :: [a] -> Int -> Gen [a]
genRemoveRandom xs 0 = return xs
genRemoveRandom [] _ = error "list to small"
genRemoveRandom xs n = do
  i <- choose (0, n-1)
  genRemoveRandom (remove i xs) (n-1)

  where
    remove _ []     = error "index to large"
    remove 0 (y:ys) = ys
    remove i (y:ys) = y : remove (i-1) ys

genScaleOffsets :: Gen [ScaleOffset]
genScaleOffsets = choose (1, 12) >>= genRemoveRandom [0,1,2,3,4,5,6,7,8,9,10,11]

prop_spnFromTo :: SPN -> Bool
prop_spnFromTo n = n == fromNote (toNote n)

prop_semiFromTo :: Note -> Bool
prop_semiFromTo n = n == toNote (fromNote n)

prop_mkSPN :: SPN -> Bool
prop_mkSPN n@(SPN o p) = n == mkSPN o (tone p) (accidental p)

prop_offsetFromTo :: ScaleOffset -> Bool
prop_offsetFromTo o = o == toOffset (fromOffset o)

prop_pitchClassFromTo :: PitchClass -> Bool
prop_pitchClassFromTo p = p == fromOffset (toOffset p)

prop_offsetsFromTo :: Property
prop_offsetsFromTo = forAll genScaleOffsets $ \offsets ->
  offsets == toOffsets (fromOffsets offsets)

prop_scaleFromTo :: Scale -> Bool
prop_scaleFromTo scale = scale == fromOffsets (toOffsets scale)

prop_chromaticAll :: Note -> Note -> Bool
prop_chromaticAll r o = hasNote o (RootedScale r chromatic)

(@=) :: Eq a => a -> a -> Assertion
a @= b = a == b @?= True

(@/=) :: Eq a => a -> a -> Assertion
a @/= b = a == b @?= False

tests :: [Test]
tests =
  [ testGroup "Note properties"
    [ testProperty "Note conversion" prop_semiFromTo
    , testProperty "SPN conversion" prop_spnFromTo
    , testProperty "SPN creation" prop_mkSPN
    , testProperty "Offset conversion" prop_offsetFromTo
    , testProperty "PitchClass conversion" prop_pitchClassFromTo
    ]
  , testGroup "Scale properties"
    [ testProperty "ScaleOffsets conversion" prop_offsetsFromTo
    , testProperty "Scale conversion" prop_scaleFromTo
    , testProperty "Chromatic has all notes" prop_chromaticAll
    ]
  , testGroup "Note values"
    [ testCase "0 is C0"  (fromNote 0 @= mkSPN 0 C Natural)
    , testCase "1 is C0#" (fromNote 1 @= mkSPN 0 C Sharp)
    , testCase "2 is D0"  (fromNote 2 @= mkSPN 0 D Natural)
    , testCase "2 is D0"  (fromNote 2 @= mkSPN 0 D Natural)
    , testCase "48 is C4"  (fromNote 48 @= mkSPN 4 C Natural)
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
