module TestSuite6 where

import Angabe6 hiding (main, spec, datenbank, persNr)
import Test.Tasty as T
import Test.Tasty.HUnit as T
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Prelude as P

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite6 Spec"
    [ genInsertTests,
      genSortTests,
      derivedFromGenSortTests,
      datenbankTests
    ]

genInsertTests :: TestTree
genInsertTests =
  testGroup
    "A.1 gen_insert"
    [ testCase "Test 1" $ do
        gen_insert (<) 0 [1, 2, 3] @?= [0, 1, 2, 3],
      testCase "Test 2" $ do
        gen_insert (<) 2 [1, 3] @?= [1, 2, 3],
      testCase "Test 3" $ do
        gen_insert (<) 3 [1, 2] @?= [1, 2, 3],
      testCase "Test 4" $ do
        gen_insert (<) 3 [] @?= [3]
    ]

genSortTests :: TestTree
genSortTests =
  testGroup
    "A.1 gen_sort"
    [ testCase "Test 1" $ do
        gen_sort (<) [] @?= ([] :: [Int]),
      testCase "Test 2" $ do
        gen_sort (<) [1] @?= [1],
      testCase "Test 3" $ do
        gen_sort (<) [3, 2, 1, 4] @?= [1, 2, 3, 4]
    ]

derivedFromGenSortTests :: TestTree
derivedFromGenSortTests =
  testGroup
    "A.3-A.11"
    [ testCase "Test auf_ord" $ do
        auf_ord [3, 2, 1, 4] @?= [1, 2, 3, 4],
      testCase "Test ab_ord" $ do
        ab_ord [3, 2, 1, 4] @?= [4, 3, 2, 1],
      testCase "Test auf_lst" $ do
        auf_lst [[1], [3, 2], []] @?= [[], [1], [3, 2]],
      testCase "Test ab_lst" $ do
        ab_lst [[1], [3, 2], []] @?= [[3, 2], [1], []],
      -- fuer tests bei funktionen muessen die funktionen nach dem sortieren ausgewertet werden
      testCase "Test auf_fun" $ do
        map (\f -> f 1) (auf_fun [id, (2 +), (2 ^)]) @?= [1, 2, 3],
      testCase "Test ab_fun" $ do
        map (\f -> f 1) (ab_fun [id, (2 +), (2 ^)]) @?= [3, 2, 1],
      testCase "Test auf_onfun" $ do
        map (\f -> f 1) (auf_onfun [id, (2.0 +), (2 **)]) @?= [1, 2, 3],
      testCase "Test ab_onfun" $ do
        map (\f -> f 1) (ab_onfun [id, (2.0 +), (2 **)]) @?= [3, 2, 1]
    ]

datenbank :: Datenbank
datenbank =
  [ P 1 "A" 25 M 100 Nothing,
    P 2 "X" 37 F 180 (Just Apple),
    P 3 "B" 52 F 153 (Just Nokia),
    P 4 "GE" 28  D 130 (Just Nokia),
    P 2 "KD" 33  M 140 (Just Apple),
    P 5 "F" 22 F 155 Nothing
  ]

persNr :: Person -> PersNummer
persNr (P n _ _ _ _ _) = n

datenbankTests :: TestTree
datenbankTests =
  testGroup
    "A.13 Datenbankansichten"
    [ testCase "Normalansicht" $ do
        map persNr (normalsicht datenbank) @?= [1, 3, 5, 4, 2, 2],
      testCase "Anlageberatungssicht" $ do
        map persNr (anlageberatungssicht datenbank) @?= [2, 5, 3, 2, 4, 1],
      testCase "Personalabteilungssicht" $ do
        map persNr (personalabteilungssicht datenbank) @?= [4, 5, 2, 3, 1, 2],
      testCase "Sozialforschungssicht" $ do
        map persNr (sozialforschungssicht datenbank) @?= [2, 2, 3, 4, 5, 1],
      testCase "Integritaetsansicht" $ do
        take 2 (map persNr (integritaetssicht datenbank)) @?= [2, 2],
      testCase "Chaossicht" $ do
        map persNr (auch_im_chaos_ist_ordnung_sicht datenbank) @?= [1, 3, 2, 4, 5, 2]
    ]
