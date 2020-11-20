module TestSuite3 where

import Angabe3 hiding (expectError, main)
import Prelude as P
import qualified Control.Exception as E
import qualified Control.Monad as M
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite2 Spec"
    [ showInstanceTests,
      eqInstanceTests,
      ordInstanceTests,
      numInstanceTests,
      enumInstanceTests,
      kanonischTests,
      elementTests,
      codeTests,
      exTestTests
    ]

showInstanceTests :: TestTree
showInstanceTests =
  testGroup
    "Show Instance Tests"
    [ testCase "Test 1" $ do
        P.show Ungueltig @?= "Kein Intervall",
      testCase "Test 2" $ do
        P.show Leer @?= "<>",
      testCase "Test 3" $ do
        P.show (IV (1, 2)) @?= "<1,2>",
      testCase "Test 4" $ do
        P.show (IV (-2, 4)) @?= "<-2,4>",
      testCase "Test 5" $ do
        P.show (IV (2, 1)) @?= "<>"
    ]

eqInstanceTests :: TestTree
eqInstanceTests =
  testGroup
    "Eq Instance Tests"
    [ testCase "Test 1" $ do
        IV (2, 5) P.== IV (2, 5) @?= True,
      testCase "Test 2" $ do
        IV (2, 5) P.== IV (5, 2) @?= False,
      testCase "Test 3" $ do
        IV (2, 5) P./= IV (2, 5) @?= False,
      testCase "Test 4" $ do
        Leer P.== IV (5, 2) @?= True,
      testCase "Test 5" $ do
        (IV (2, 5) P./= Ungueltig) `expectError` "Vergleich nicht moeglich",
      testCase "Test 6" $ do
        (Leer P.== Ungueltig) `expectError` "Vergleich nicht moeglich"
    ]

ordInstanceTests :: TestTree
ordInstanceTests =
  testGroup
    "Ord Instance Tests"
    [ testCase "Test 1" $ do
        IV (2, 5) P.<= IV (2, 5) @?= True,
      testCase "Test 2" $ do
        IV (2, 5) P.> IV (1, 4) @?= False,
      testCase "Test 3" $ do
        IV (2, 3) P.<= IV (2, 5) @?= True,
      testCase "Test 4" $ do
        IV (3, 4) P.< IV (2, 5) @?= True,
      testCase "Test 5" $ do
        IV (3, 5) P.< IV (2, 4) @?= False,
      testCase "Test 6" $ do
        (IV (2, 5) P.<= Ungueltig) `expectError` "Vergleich nicht moeglich",
      testCase "Test 7" $ do
        (Leer P.> Ungueltig) `expectError` "Vergleich nicht moeglich"
    ]

numInstanceTests :: TestTree
numInstanceTests =
  testGroup
    "Num Instance Tests"
    [ testCase "Test 1" $ do
        IV (2, 5) P.+ IV (2, 5) @?= IV (4, 10),
      testCase "Test 2" $ do
        IV (2, 5) P.+ IV (-1, -1) @?= IV (1, 4),
      testCase "Test 3" $ do
        (IV (0, 0) P.+ Ungueltig) `expectError` "Vergleich nicht moeglich",
      testCase "Test 4" $ do
        IV (2, 5) P.- IV (2, 5) @?= IV (-3, 3),
      testCase "Test 5" $ do
        IV (2, 5) P.- IV (-5, -1) @?= IV (3, 10),
      testCase "Test 6" $ do
        (Ungueltig P.- IV (0, 0)) `expectError` "Vergleich nicht moeglich",
      testCase "Test 7" $ do
        IV (2, 5) P.* IV (2, 5) @?= IV (4, 25),
      testCase "Test 8" $ do
        IV (2, 5) P.* IV (-5, -1) @?= IV (-25, -2),
      testCase "Test 9" $ do
        (Ungueltig P.* IV (0, 0)) `expectError` "Vergleich nicht moeglich",
      testCase "Test 10" $ do
        P.abs (IV (2, 5)) @?= IV (2, 5),
      testCase "Test 11" $ do
        P.abs (IV (-5, -2)) @?= IV (2, 5),
      testCase "Test 12" $ do
        P.abs (IV (-2, 5)) @?= IV (0, 5),
      testCase "Test 13" $ do
        P.abs (IV (5, 2)) @?= Leer,
      testCase "Test 14" $ do
        P.abs Ungueltig `expectError` "Vergleich nicht moeglich"
    ]

enumInstanceTests :: TestTree
enumInstanceTests =
  testGroup
    "Enum Instance Tests"
    [ testCase "Test 1" $ do
        P.toEnum 5 @?= IV (5, 5),
      testCase "Test 2" $ do
        P.fromEnum (IV (5, 5)) @?= 5,
      testCase "Test 3" $ do
        P.fromEnum (IV (2, 5)) `expectError` "Operation nicht moeglich",
      testCase "Test 4" $ do
        P.fromEnum (IV (5, 2)) `expectError` "Operation nicht moeglich",
      testCase "Test 5" $ do
        P.fromEnum Ungueltig `expectError` "Operation nicht moeglich"
    ]

kanonischTests :: TestTree
kanonischTests =
  testGroup
    "Kanonisch Tests"
    [ testCase "Test 1" $ do
        kanonisch (IV (2, 5)) @?= IV (2, 5),
      testCase "Test 2" $ do
        kanonisch (IV (5, 2)) @?= Leer,
      testCase "Test 3" $ do
        kanonisch Leer @?= Leer,
      testCase "Test 4" $ do
        case kanonisch Ungueltig of
          Ungueltig -> pure ()
          _ -> assertFailure "Expected \"Ungueltig\""
    ]

elementTests :: TestTree
elementTests =
  testGroup
    "Element Tests"
    [ testCase "Test 1" $ do
        is_elem 3 (IV (2, 5)) @?= Just True,
      testCase "Test 2" $ do
        is_elem 3 Leer @?= Just False,
      testCase "Test 3" $ do
        is_elem 3 Ungueltig @?= Nothing,
      testCase "Test 4" $ do
        is_elem 0 (IV (2, 5)) @?= Just False
    ]

codeTests :: TestTree
codeTests =
  testGroup
    "Code Tests"
    [ testCase "Test 1" $ do
        codiere [2 .. 5] @?= IV (2, 5),
      testCase "Test 2" $ do
        case codiere [2, 3, 4, 5, 4] of
          Ungueltig -> pure ()
          _ -> assertFailure "Expected \"Ungueltig\"",
      testCase "Test 3" $ do
        codiere [] @?= Leer,
      testCase "Test 4" $ do
        codiere [1] @?= IV (1, 1),
      testCase "Test 5" $ do
        decodiere (IV (2, 5)) @?= Just [2 .. 5],
      testCase "Test 6" $ do
        decodiere Ungueltig @?= Nothing,
      testCase "Test 7" $ do
        decodiere Leer @?= Just [],
      testCase "Test 8" $ do
        decodiere (IV (5, 2)) @?= Just [],
      testCase "Test 9" $ do
        decodiere (IV (1, 1)) @?= Just [1]
    ]

exTestTests :: TestTree
exTestTests =
  testGroup
    "ExTest Tests"
    [ testCase "Test 1" $ do
        extrahiere (Just [(2 :: Int) .. 5]) @?= [2 .. 5],
      testCase "Test 2" $ do
        extrahiere (Nothing :: Maybe [Int]) `expectError` "Extraktion nicht moeglich",
      testCase "Test 3" $ do
        extrahiere (Just [(5 :: Int), 4 .. 2]) @?= [5, 4 .. 2],
      testCase "Test 4" $ do
        ist_aufsteigend ([1] :: [Int]) @?= True,
      testCase "Test 5" $ do
        ist_aufsteigend ([1 .. 3] :: [Int]) @?= True,
      testCase "Test 6" $ do
        ist_aufsteigend ([1, 2, 3, 2] :: [Int]) @?= False,
      testCase "Test 7" $ do
        ist_lueckenlos ([1] :: [Int]) @?= True,
      testCase "Test 8" $ do
        ist_lueckenlos ([1 .. 3] :: [Int]) @?= True,
      testCase "Test 9" $ do
        ist_lueckenlos ([1, 2, 3, 5] :: [Int]) @?= False,
      testCase "Test 7" $ do
        ist_laL_Element (1 :: Int) Nothing @?= False,
      testCase "Test 8" $ do
        ist_laL_Element (1 :: Int) (Just [2, 3, 4]) @?= False,
      testCase "Test 9" $ do
        ist_laL_Element (1 :: Int) (Just [1, 2, 4]) @?= False,
      testCase "Test 10" $ do
        ist_laL_Element (1 :: Int) (Just [1, 2, 3, 4]) @?= True
    ]

expectError :: Show a => a -> String -> Assertion
expectError val expectedMsg = do
  err <- E.try (E.evaluate val)
  case err of
    Left (E.ErrorCall actual) ->
      M.unless (expectedMsg P.== actual) $
        T.assertFailure $
          "expected: " ++ expectedMsg ++ "\n but got: " ++ actual
    Right r -> do
      T.assertFailure $ "Expected an exception but got: " ++ P.show r
