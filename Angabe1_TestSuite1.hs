
import Angabe1 hiding (main)
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
    [ filtereTests,
      kommtVorTests,
      ausTests,
      hTests
    ]

filtereTests :: TestTree
filtereTests =
  testGroup
    "Filtere Tests"
    [ testCase "Test 1" $ do
        filtere 5 [1 .. 5] @?= [],
      testCase "Test 2" $ do
        filtere 1 [1 .. 5] @?= [5, 4 .. 1],
      testCase "Test 3" $ do
        filtere 2 [2, 2, 2, 3, 3, 4, 5, 5] @?= [5, 3],
      testCase "Test 4" $ do
        filtere 5 (replicate 5 1) @?= [1],
      testCase "Test 5" $ do
        filtere 3 [4, -2, 5, 4, 3, -2, 5, 4, -12] @?= [4]
    ]

kommtVorTests :: TestTree
kommtVorTests =
  testGroup
    "KommtVor Tests"
    [ testCase "Test 1" $ do
        kommt_vor 1 [] @?= False,
      testCase "Test 2" $ do
        kommt_vor 1 [(1, 1)] @?= True,
      testCase "Test 3" $ do
        kommt_vor 4 [(1, 2), (3, 4)] @?= True,
      testCase "Test 4" $ do
        kommt_vor 3 [(1, 2), (4, 5)] @?= False,
      testCase "Test 5" $ do
        kommt_vor (-10) [(1, 2), (3, 4), (-10, -8), (-6, 5)] @?= True
    ]

ausTests :: TestTree
ausTests =
  testGroup
    "Aus Tests"
    [ testCase "Test 1" $ do
        aus [] @?= [],
      testCase "Test 2" $ do
        aus [5,4..1] @?= [1..5],
      testCase "Test 3" $ do
        aus [1,1, 2] @?= [1,1,2,2],
      testCase "Test 4" $ do
        aus [-12, 3, 4, 1, -12, 3, 5, 5, 5] @?= concatMap (replicate 3) [-12, 1, 3, 4, 5]
    ]

hTests :: TestTree
hTests =
  testGroup
    "H Tests"
    [testCase "Test 1" $ do
        h "" "" @?= 0,
      testCase "Test 2" $ do
        h "FooBar" "FooBar" @?= 0,
      testCase "Test 3" $ do
        h "Different" "Size" @?= (-1),
      testCase "Test 4" $ do
        h "Racecar" "racecaR" @?= 2
    ]
