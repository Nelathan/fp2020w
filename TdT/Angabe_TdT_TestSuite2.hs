{-
 - Wie kann diese TestSuite ausgeführt werden?
 -
 - Am Server g0 kann die TestSuite ausgeführt werden über:
 -
 - > cabal repl -b base -b tasty -b tasty-hunit
 - > :l Angabe_TdT_TestSuite1.hs
 - > main
 -
 - Lokal funktioniert es genauso.
 - Um die Tests *lokal* mit stack auszuführen:
 -
 - > stack ghci --package base --package tasty --package tasty-hunit
 - > :l Angabe_TdT_TestSuite1.hs
 - > main
 -
 - Bitte beachten sich, dass sich die Abgabe direkt neben der TestSuite befinden muss!
-}

import Angabe_TdT
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite1 Spec"
    [ fakultaetTests,
      fibonacciTests,
      binomTests,
      fakultaetFehlerhaftTests,
      fibonacciFehlerhaftTests,
      binomFehlerhaftTests,
      binomLangsamTests
    ]

fakultaetTests :: TestTree
fakultaetTests =
  testGroup
    "Fakultät Tests"
    [ testCase "Fakultaet Test 0" $
        fakultaet 0 @?= 1,
      testCase "Fakultaet Test 1" $
        fakultaet 1 @?= 1,
      testCase "Fakultaet Test 10" $
        fakultaet 15 @?= 1307674368000,
      testCase "Fakultaet Test 30" $
        fakultaet 30 @?= 265252859812191058636308480000000
    ]

fibonacciTests :: TestTree
fibonacciTests =
  testGroup
    "Fibonacci Tests"
    [ testCase "Fibonacci Test 0" $
        fibonacci 0 @?= 0,
      testCase "Basic Fib Test 1" $
        fibonacci 1 @?= 1,
      testCase "Basic Fib Test 10" $
        fibonacci 10 @?= 55,
      testCase "Basic Fib Test 15" $
        fibonacci 15 @?= 610
    ]

binomTests :: TestTree
binomTests =
  testGroup
    "Binom Tests"
    [ testCase "Basic Binom Test 1" $
        binom (10, 0) @?= 1,
      testCase "Basic Binom Test 2" $
        binom (10, 1) @?= 10,
      testCase "Basic Binom Test 3" $
        binom (21, 5) @?= 20349,
      testCase "Big Binom Test 1" $
        binom (50, 25) @?= 126410606437752
    ]

fakultaetFehlerhaftTests :: TestTree
fakultaetFehlerhaftTests =
  testGroup
    "Fakultät_Fehlerhaft Tests"
    [ testCase "Fakultaet Test 0" $
        fak_fehlerhaft 0 @?= 1,
      testCase "Fakultaet Test 1" $
        fak_fehlerhaft 1 @?= 1,
      testCase "Fakultaet Test 10" $
        fak_fehlerhaft 15 @?= 1307674368000,
      testCase "Fakultaet Test 30" $
        fak_fehlerhaft 30 @?= 265252859812191058636308480000000
    ]

fibonacciFehlerhaftTests :: TestTree
fibonacciFehlerhaftTests =
  testGroup
    "Fibonacci_Fehlerhaft tests"
    [ testCase "Fibonacci Test 0" $
        fib_fehlerhaft 0 @?= 0,
      testCase "Basic Fib Test 1" $
        fib_fehlerhaft 1 @?= 1,
      testCase "Basic Fib Test 10" $
        fib_fehlerhaft 10 @?= 55,
      testCase "Basic Fib Test 15" $
        fib_fehlerhaft 15 @?= 610
    ]

binomFehlerhaftTests :: TestTree
binomFehlerhaftTests =
  testGroup
    "Binom_Fehlerhaft tests"
    [ testCase "Basic Binom Test 1" $
        binom_fehlerhaft (10, 0) @?= 1,
      testCase "Basic Binom Test 2" $
        binom_fehlerhaft (10, 1) @?= 10,
      testCase "Basic Binom Test 3" $
        binom_fehlerhaft (21, 5) @?= 20349,
      testCase "Big Binom Test 1" $
        binom_fehlerhaft (50, 25) @?= 126410606437752
    ]

binomLangsamTests :: TestTree
binomLangsamTests =
  testGroup
    "Binom_Langsam tests"
    [ testCase "Basic Binom Test 1" $
        binom_langsam (10, 0) @?= 1,
      testCase "Basic Binom Test 2" $
        binom_langsam (10, 1) @?= 10,
      testCase "Basic Binom Test 3" $
        binom_langsam (21, 5) @?= 20349,
      testCase "Big Binom Test 1" $
        binom_langsam (50, 25) @?= 126410606437752
    ]
