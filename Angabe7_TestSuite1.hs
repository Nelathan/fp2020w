module TestSuite7 where

import Angabe7 hiding (leeresBand, main, spec)
import qualified Control.Exception as E
import Data.Maybe (isNothing)
import System.Timeout (timeout)
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
    "TestSuite7 Spec"
    [ aktRechenbandTests,
      wandleRbTests,
      zulaessigeTuringtafelTest,
      transitionTest,
      spurTest,
      zeigeZustandTest,
      simTest,
      simShowTest
    ]

leeresBand :: Rechenband
leeresBand = RB U (const Blank)

aktRechenbandTests :: TestTree
aktRechenbandTests =
  testGroup
    "A.1 akt_rechenband"
    [ testCase "Test 1" $ do
        let RB bounds band = akt_rechenband leeresBand 0 Blank
        bounds @?= U
        band 0 @?= Blank,
      testCase "Test 2" $ do
        let RB bounds band = akt_rechenband leeresBand 1 $ Z 'X'
        bounds @?= B 1 1
        band 1 @?= Z 'X',
      testCase "Test 3" $ do
        let rb = akt_rechenband leeresBand 1 $ Z 'X'
        let RB bounds band = akt_rechenband rb 1 Blank
        bounds @?= U
        band 1 @?= Blank,
      testCase "Test 4" $ do
        let rb1 = akt_rechenband leeresBand 1 $ Z 'X'
        let rb2 = akt_rechenband rb1 2 $ Z 'Y'
        let RB bounds band = akt_rechenband rb2 2 Blank
        bounds @?= B 1 1
        band 1 @?= Z 'X'
        band 2 @?= Blank
    ]

wandleRbTests :: TestTree
wandleRbTests =
  testGroup
    "A.2 a) wandle_in_rb"
    [ testCase "Test 1" $ do
        let RB bounds band = wandle_in_rb "123"
        bounds @?= B 1 3
        map band [1, 2, 3] @?= [Z '1', Z '2', Z '3']
        band 0 @?= Blank,
      testCase "Test 2" $ do
        let RB bounds _ = wandle_in_rb []
        bounds @?= U
    ]

zulaessigeTuringtafelTest :: TestTree
zulaessigeTuringtafelTest =
  testGroup
    "A.2 b) ist_zulaessige_Turingtafel"
    [ testCase "Test 1" $ do
        ist_zulaessige_Turingtafel
          [ (1, Blank, Drucke Blank, 1),
            (2, Z 'X', Bewege_LSK_nach Links, 1),
            (1, Blank, Bewege_LSK_nach Links, 2)
          ]
          @?= False,
      testCase "Test 2" $ do
        ist_zulaessige_Turingtafel
          [ (1, Blank, Drucke Blank, 1),
            (2, Z 'A', Bewege_LSK_nach Links, 1),
            (1, Z 'A', Bewege_LSK_nach Links, 2)
          ]
          @?= True
    ]

transitionTest :: TestTree
transitionTest =
  testGroup
    "A.2 c) transition"
    [ testCase "Test 1" $ do
        let GZ _ (RB _ band) z pos = transition (GZ [(1, Blank, Drucke $ Z 'X', 2)] leeresBand 1 0)
        pos @?= 0
        z @?= 2
        band 0 @?= Z 'X',
      testCase "Test 2" $ do
        let GZ _ (RB bounds band) z pos = transition (GZ [(1, Blank, Bewege_LSK_nach Rechts, 2)] leeresBand 1 0)
        pos @?= 1
        z @?= 2
        bounds @?= U
        band 0 @?= Blank
        band 1 @?= Blank,
      testCase "Test 3" $ do
        let GZ _ (RB bounds _) z pos = transition (GZ [(1, Z 'A', Bewege_LSK_nach Rechts, 2)] leeresBand 1 0)
        pos @?= 0
        z @?= 1
        bounds @?= U
    ]

spurTest :: TestTree
spurTest =
  testGroup
    "A.2 d) spur"
    [ testCase "Test 1" $ do
        GZ _ (RB bounds band) _ pos <-
          E.evaluate $
            spur (GZ [(1, Blank, Drucke $ Z 'X', 1), (1, Z 'X', Bewege_LSK_nach Rechts, 1)] leeresBand 1 1)
              !! 5
        map band [1 .. 3] @?= [Z 'X', Z 'X', Z 'X']
        bounds @?= B 1 3
        pos @?= 3,
      testCase "Test 2" $ do
        testSpur <-
          E.evaluate $ spur (GZ [(1, Blank, Drucke $ Z 'X', 1), (1, Z 'X', Drucke $ Z 'Y', 1)] leeresBand 1 1)
        length testSpur @?= 3
        let GZ _ (RB bounds band) _ pos = last testSpur
        bounds @?= B 1 1
        band 1 @?= Z 'Y'
        pos @?= 1
    ]

zeigeZustandTest :: TestTree
zeigeZustandTest =
  testGroup
    "A.2 e) zeige_zustand"
    [ testCase "Test 1" $ do
        zeige_zustand (GZ [] leeresBand 1 1) @?= "(IZ:1,LSK:1,B:unbeschrieben)",
      testCase "Test 2" $ do
        zeige_zustand (GZ [] (wandle_in_rb "XYZ") 0 2) @?= "(IZ:0,LSK:2,B:XYZ,Min:1,Max:3)"
    ]

simTest :: TestTree
simTest =
  testGroup
    "A.2 g) sim"
    [ testCase "Test 1" $ do
        let SA _ _ (RB bounds band) =
              sim $
                SE
                  [ (0, Blank, Drucke $ Z 'X', 0),
                    (0, Z 'X', Bewege_LSK_nach Links, 1),
                    (0, Z 'Y', Bewege_LSK_nach Rechts, 1),
                    (1, Blank, Drucke $ Z 'Y', 0),
                    (1, Z 'X', Drucke $ Z 'Z', 3)
                  ]
                  leeresBand
        bounds @?= B (-1) 0
        map band [(-1) .. 0] @?= [Z 'Y', Z 'Z'],
      testCase "Test 2" $ do
        result <- timeout 1000000 $ do
          let SA _ _ pos =
                sim $
                  SE
                    [(0, Blank, Bewege_LSK_nach Links, 0)]
                    leeresBand
          pos `seq` return ()
        assertBool "simulation sollte nicht terminieren" $ isNothing result
    ]

simShowTest :: TestTree
simShowTest =
  testGroup
    "A.2 h) instance Show Sim_Ausgabe"
    [ testCase "Test 1" $ do
        show (SA 1 2 leeresBand) @?= "IZ: 1 // LSKP: 2 // BI: Leer",
      testCase "Test 2" $ do
        let rb = akt_rechenband leeresBand 0 $ Z 'X'
        show (SA 0 1 rb) @?= "IZ: 0 // LSKP: 1 // BI: 0>X<0"
    ]
