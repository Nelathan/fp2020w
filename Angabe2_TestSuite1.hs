module TestSuite2 where

import Angabe2 hiding (main)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite2 Spec"
    [ ist_einsbpTests,
      anz01bpsTests,
      liefere_woerter_inTests,
      hMTests
    ]

ist_einsbpTests :: TestTree
ist_einsbpTests =
  testGroup
    "IstEinsBinaerPrim Tests"
    [ testCase "Test 2" $ do
        ist_einsbp 1 @?= Nein,
      testCase "Test 3" $ do
        ist_einsbp 3 @?= Ja,
      testCase "Test 4" $ do
        ist_einsbp 505 @?= Ja,
      testCase "Test 5" $ do
        ist_einsbp 5999 @?= Nein
    ]

anz01bpsTests :: TestTree
anz01bpsTests =
  testGroup
    "anz01bps Tests"
    [ testCase "Test 1" $ do
        anz01bps (0, 5) @?= (1, 2),
      testCase "Test 2" $ do
        anz01bps (5, 0) @?= (-1, -1),
      testCase "Test 3" $ do
        anz01bps (5, 5) @?= (0, 1),
      testCase "Test 4" $ do
        anz01bps (15, 25) @?= (8, 8),
      testCase "Test 5" $ do
        anz01bps (0, 500) @?= (297, 277)
    ]

liefere_woerter_inTests :: TestTree
liefere_woerter_inTests =
  testGroup
    "liefere_woerter_in Tests"
    [ testCase "Test 1" $ do
        liefere_woerter_in "Functional Programming is Fun" @?= ["Functional", "Programming", "is", "Fun"],
      testCase "Test 2" $ do
        liefere_woerter_in "" @?= [],
      testCase "Test 3" $ do
        liefere_woerter_in "\tHello,\nWorld" @?= ["Hello,", "World"],
      testCase "Test 4" $ do
        liefere_woerter_in "Monad's are\n \tjust like burritos\n" @?= ["Monad's", "are", "just", "like", "burritos"]
    ]

hMTests :: TestTree
hMTests =
  testGroup
    "hM Tests"
    [ testCase "Test 1" $ do
        hM [] @?= -1,
      testCase "Test 2" $ do
        hM ["foo"] @?= -1,
      testCase "Test 3" $ do
        hM ["ab", "aa", "bb"] @?= 1,
      testCase "Test 4" $ do
        hM ["Haskell", "Fortran", "Miranda", "Clojure"] @?= 6,
      testCase "Test 4" $ do
        hM ["Unterschiedlich", "lang", "Miranda", "Clojure"] @?= -1
    ]
