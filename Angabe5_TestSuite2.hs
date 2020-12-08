module TestSuite5 where

import Angabe5 hiding (main, spec, wahlvorschlag)
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
    "TestSuite5 Spec"
    [
      wahlvorschlagGueltigTests,
      stimmzettelGueltigTests,
      trenneStimmzettelTests,
      auszaehlenTests,
      wahlsiegerTests,
      ausscheidenTests,
      wahlausgangTests1,
      wahlausgangTests2,
      wahlanalyseTests
    ]

wahlvorschlag :: Wahlvorschlag
wahlvorschlag = [WW "John" "Smith" ABC, WW "Judy" "Hall" DEF, WW "John" "Doug" MNO]

wahlvorschlagGueltigTests :: TestTree
wahlvorschlagGueltigTests =
  testGroup
    "A.1 gueltiger Wahlvorschlag"
    [ testCase "ungueltig" $ do
        ist_gueltiger_Wahlvorschlag [] @?= False,
      testCase "gueltig" $
        ist_gueltiger_Wahlvorschlag wahlvorschlag @?= True
    ]

stimmzettelGueltigTests :: TestTree
stimmzettelGueltigTests =
  testGroup
    "A.2 gueltiger Stimmzettel"
    [ testCase "Test 1" $ do
        ist_gueltiger_Stimmzettel wahlvorschlag [1, 2, 3] @?= True,
      testCase "Test 2" $
        ist_gueltiger_Stimmzettel wahlvorschlag [] @?= True,
      testCase "Test 3" $
        ist_gueltiger_Stimmzettel wahlvorschlag [1, 1, 2] @?= False,
      testCase "Test 4" $
        ist_gueltiger_Stimmzettel wahlvorschlag [4] @?= False,
      testCase "Test 5" $
        ist_gueltiger_Stimmzettel wahlvorschlag [10] @?= False
    ]

trenneStimmzettelTests :: TestTree
trenneStimmzettelTests =
  testGroup
    "A.3 trenne Stimmzettel"
    [ testCase "Test 1" $ do
        trenne_Stimmzettel wahlvorschlag [[2, 1, 3], [2, 10], [1, 1, 2], []] @?= ([[2, 1, 3], []], [[2, 10], [1, 1, 2]])
    ]

auszaehlenTests :: TestTree
auszaehlenTests =
  testGroup
    "A.4 Auszaehlen"
    [ testCase "Test 1" $ do
        auszaehlen [] [] @?= Nothing,
      testCase "Test 2" $ do
        auszaehlen wahlvorschlag [[1, 2, 3], [1, 2], [3]] @?= Just [2, 0, 1],
      testCase "Test 3" $ do
        auszaehlen wahlvorschlag [[2, 1], [3, 2], [1], [3, 2], [1, 2, 3], [1, 2], [3, 1], [3]] @?= Just [3, 1, 4]
    ]

wahlsiegerTests :: TestTree
wahlsiegerTests =
  testGroup
    "A.5 Wahlsieger"
    [ testCase "Test 1" $ do
        wahlsieger wahlvorschlag (Just [2, 4, 1]) @?= Just (wahlvorschlag !! 1, 2),
      testCase "Test 2" $ do
        wahlsieger wahlvorschlag (Just [2, 1, 1]) @?= Nothing,
      testCase "Test 3" $ do
        wahlsieger wahlvorschlag (Just [1, 2, 3]) @?= Nothing
    ]

ausscheidenTests :: TestTree
ausscheidenTests =
  testGroup
    "A.6 Ausscheiden"
    [ testCase "Test 1" $ do
        filter (not . null) (ausscheiden [[1, 2], [3, 1], [3], [2, 3], [1, 3]] [2, 1, 2])
          @?= [[1], [3, 1], [3], [3], [1, 3]],
      testCase "Test 2" $ do
        filter (not . null) (ausscheiden [[1, 2], [3, 1], [2, 3]] [1, 1, 1]) @?= []
    ]

wahlausgangTests1 :: TestTree
wahlausgangTests1 =
  testGroup
    "A.7 Wahlausgang, Teil 1"
    [ testCase "Test 1" $ do
        wahlausgang [] [[]] @?= Ungueltiger_Wahlvorschlag,
      testCase "Test 2" $ do
        wahlausgang wahlvorschlag [[1, 1]] @?= Keine_gueltigen_Stimmen
    ]

wahlausgangTests2 :: TestTree
wahlausgangTests2 =
  testGroup
    "A.7 Wahlausgang, Teil 2"
    [ testCase "Test 1" $
        wahlausgang
          wahlvorschlag
          [ [1, 2],
            [3, 1, 2],
            [2, 3, 1],
            [3],
            [1, 2]
          ]
          @?= Gewaehlt_ist (wahlvorschlag !! 2),
      testCase "Test 2" $ do
        wahlausgang
          wahlvorschlag
          [ [1, 2],
            [3, 1, 2],
            [2, 3, 1],
            [3],
            [1, 2],
            [2, 1],
            [3, 2],
            [1, 3]
          ]
          @?= Kein_Wahlsieger_Wahlwiederholung
    ]

wahlanalyseTests :: TestTree
wahlanalyseTests =
  testGroup
    "A.8 Wahlanalyse"
    [ testCase "Test 1" $ do
        wahlanalyse [head wahlvorschlag] [[1]] @?= Keine,
      testCase "Test 2" $
        wahlanalyse
          wahlvorschlag
          [ [1, 2],
            [3, 1, 2],
            [2, 3, 1],
            [3],
            [1, 2],
            [2, 1],
            [3, 2],
            [1, 3]
          ]
          @?= GWV [DEF],
      testCase "Test 3" $
        wahlanalyse
          wahlvorschlag
          [ [3, 2],
            [3, 1, 2],
            [2, 3, 1],
            [3],
            [1, 2],
            [2, 1],
            [3, 2],
            [1, 3]
          ]
          @?= GWV [ABC, DEF]
    ]
