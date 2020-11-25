module TestSuite4 where

import Angabe4 hiding (main)
import Prelude as P
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite4 Spec"
    [ breitestTests,
      taeTests,
      awiTests,
      showTests,
      ordTests
    ]

breitestTests :: TestTree
breitestTests =
  testGroup
    "Tests `breitest`"
    [ testCase "Test 1" $ do
        breitest (Blatt "") @?= Ausw 1 [0],
      testCase "Test 2" $ do
        breitest (Knoten "" (Knoten "" (Blatt "") (Blatt "")) (Blatt "")) @?= Ausw 2 [1, 2],
      testCase "Test 3" $ do
        breitest
          (Knoten ""
            (Knoten ""
              (Blatt "")
              (Blatt "")
            )
            (Knoten ""
              (Knoten ""
                (Knoten "" (Blatt "") (Blatt ""))
                (Knoten "" (Blatt "") (Blatt ""))
              )
              (Blatt "")
            )
          )
          @?= Ausw 4 [2, 4]
    ]

taeTests :: TestTree
taeTests =
  testGroup
    "Tests `tae`"
    [ testCase "Test 1" $ do
        tae (Blatt "X") 0 @?= Just ["X"],
      testCase "Test 2" $ do
        tae (Knoten "X" (Blatt "Y") (Blatt "Z")) 1 @?= Just ["Y", "Z"],
      testCase "Test 3" $ do
        tae (Blatt "") 1 @?= Nothing,
      testCase "Test 4" $
        tae (Knoten "1" (Knoten "x" (Blatt "a") (Blatt "b")) (Blatt "2")) 1 @?= Just ["x", "2"],
      testCase "Test 5" $ do
        tae (Knoten "" (Blatt "") (Blatt "")) 2 @?= Nothing
    ]

awiTests :: TestTree
awiTests =
  testGroup
    "Tests `awi`"
    [ testCase "Test 1" $ do
        awi TB @?= TB' [],
      testCase "Test 2" $ do
        awi (TK TB TB TB) @?= TK' [] (TB' [L]) (TB' [M]) (TB' [R]),
      testCase "Test 3" $ do
        awi (TK (TK TB TB TB) (TK TB TB TB) (TK TB TB TB)) @?=
          TK' []
            (TK' [L] (TB' [L,L]) (TB' [L,M]) (TB' [L,R]))
            (TK' [M] (TB' [M,L]) (TB' [M,M]) (TB' [M,R]))
            (TK' [R] (TB' [R,L]) (TB' [R,M]) (TB' [R,R])),
      testCase "Test 4" $ do
        awi (TK (TK TB (TK TB TB (TK TB TB TB)) TB) TB TB) @?=
          TK' []
            (TK' [L]
              (TB' [L, L])
              (TK' [L, M]
                (TB' [L, M, L])
                (TB' [L, M, M])
                (TK' [L, M, R]
                  (TB' [L, M, R, L])
                  (TB' [L, M, R, M])
                  (TB' [L, M, R, R])
                )
              )
              (TB' [L, R])
            )
            (TB' [M])
            (TB' [R])
    ]

showTests :: TestTree
showTests =
  testGroup
    "Show Instance Tests"
    [ testCase "Test 1" $ do
        P.show (B ([] :: [Int])) @?= "<[]>",
      testCase "Test 2" $ do
        P.show (B [1,2]) @?= "<[1,2]>",
      testCase "Test 3" $ do
        P.show (K (B [1]) [2] (B [3])) @?= "<Wurzel [2] <[1]> <[3]>>",
      testCase "Test 4" $ do
        P.show (K (K (B [1]) [2] (B [3])) [4] (B [5])) @?= "<Wurzel [4] <Wurzel [2] <[1]> <[3]>> <[5]>>"
    ]

ordTests :: TestTree
ordTests =
  testGroup
    "Ord Instance Tests"
    [ testCase "Test 1" $ do
        P.compare (B [1]) (K (B []) [1, 2] (B [])) @?= P.LT,
      testCase "Test 2" $ do
        P.compare (K (B []) [1, 2] (B [])) (B [1]) @?= P.GT,
      testCase "Test 3" $ do
        P.compare (K (B [1]) [] (B [2])) (K (B [1]) [] (B [2]))  @?= P.EQ
    ]
