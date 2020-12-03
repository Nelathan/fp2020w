module Angabe7 where

import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Debug.Trace (trace)

type Nat0 = Integer

type Zett = Integer

type Zeichenvorrat = Char

data Bandalphabet
  = Z Zeichenvorrat
  | Blank
  deriving (Eq, Show)

type Bandfeld = Zett

type Band = (Bandfeld -> Bandalphabet)

type Min = Zett -- Kleinster Index eines beschriebenen Bandfelds

type Max = Zett -- Groesster Index eines beschriebenen Bandfelds

data MinMax
  = B Min Max -- B wie ‘Beschrieben’
  | U -- U wie ‘Unbeschrieben’
  deriving (Eq, Show)

data Rechenband = RB MinMax Band

test :: Show a => a -> a
test a = trace ('>':show a) a

-- Aufgabe A.1
leer_band :: p -> Bandalphabet
leer_band _ = Blank

leer_rb :: Rechenband
leer_rb = RB U leer_band

akt_band :: Band -> Bandfeld -> Bandalphabet -> Band
akt_band band feld zeichen x
  | x == feld = zeichen
  | otherwise = band x

akt_rechenband :: Rechenband -> Bandfeld -> Bandalphabet -> Rechenband
akt_rechenband (RB range band) feld zeichen = RB new_range (akt_band band feld zeichen)
  where
    (B lo hi) = range
    new_range
      | zeichen == Blank && range == U = U
      | zeichen == Blank && feld == lo && feld == hi = U
      | zeichen == Blank && feld == lo = B (scan [(lo + 1) .. hi]) hi
      | zeichen == Blank && feld == hi = B lo (scan $ reverse [lo .. (hi - 1)])
      | zeichen /= Blank && range == U = B feld feld
      | zeichen /= Blank && (feld < lo || feld > hi) = B (min lo feld) (max hi feld)
      | otherwise = range
    scan (m : ms) = if band m == Blank then scan ms else m

readRB :: Rechenband -> Band
readRB (RB _ band) = band

-- Aufgabe A.2

-- Lese- und Schreibkopf (LSK)
type LSK_Position = Zett

type Zeichen_unter_LSK = Bandalphabet

data Richtung
  = Links
  | Rechts
  deriving (Eq, Show)

data Befehl
  = Drucke Bandalphabet
  | Bewege_LSK_nach Richtung
  deriving (Eq, Show)

-- Interne Turingmaschinenzustaende
type Zustand = Nat0

type Interner_Zustand = Zustand

type Interner_Folgezustand = Zustand

-- Abkuerzungen
type LSKZ = Zeichen_unter_LSK

type IZ = Interner_Zustand

type IFZ = Interner_Folgezustand

-- Turing-Tafeln
type Zeile = (IZ, LSKZ, Befehl, IFZ)

type Turingtafel = [Zeile]

-- Globale Turingmaschinenzustaende
data GZustand = GZ Turingtafel Rechenband IZ LSK_Position

-- Komfortfunktion zur Vereinfachung der Turingmaschineneingabe
wandle_in_rb :: [Zeichenvorrat] -> Rechenband
wandle_in_rb cs
  | null cs = RB U leer_band
  | otherwise = RB (B 1 csl) retC
  where
    csl = fromIntegral $ length cs
    retC :: Band
    retC x
      | x > 0 && x <= csl = Z $ cs !! fromIntegral (x - 1)
      | otherwise = Blank

getIZ :: Zeile -> Interner_Zustand
getIZ (iz, _, _, _) = iz

getLSKZ :: Zeile -> Bandalphabet
getLSKZ (_, lskz, _, _) = lskz

getBefehl :: Zeile -> Befehl
getBefehl (_, _, b, _) = b

getIFZ :: Zeile -> Interner_Folgezustand
getIFZ (_, _, _, ifz) = ifz

eqZeile :: Zeile -> Zeile -> Bool
eqZeile (iz, c, _, _) (iz2, c2, _, _) = iz == iz2 && c == c2

testZeile :: Interner_Zustand -> LSKZ -> Zeile -> Bool
testZeile iz c (iz2, c2, _, _) = iz == iz2 && c == c2

isMove :: Befehl -> Bool
isMove (Bewege_LSK_nach _) = True
isMove _ = False

-- Zulaessige Turingtafeln
ist_zulaessige_Turingtafel :: Turingtafel -> Bool
ist_zulaessige_Turingtafel [] = False
ist_zulaessige_Turingtafel (l : ls)
  | null ls = True
  | otherwise = null [True | l2 <- ls, eqZeile l l2] && ist_zulaessige_Turingtafel ls

-- Turingmaschinenzustandsuebergangsfunktion, kurz Transitionsfunktion
transition :: GZustand -> GZustand
transition (GZ tafel rb iz pos)
  | isNothing next = GZ tafel rb iz pos
  | isMove befehl = GZ tafel rb ifz new_pos
  | otherwise = GZ tafel (akt_rechenband rb pos druckz) ifz pos
  where
    next = matchZeile tafel iz (readRB rb pos)
    befehl = getBefehl $ fromJust next
    ifz = getIFZ $ fromJust next
    druckz = (\(Drucke z) -> z) befehl
    new_pos = moveLSK befehl pos

matchZeile :: Turingtafel -> Interner_Zustand -> LSKZ -> Maybe Zeile
matchZeile [] _ _ = Nothing
matchZeile (t : ts) iz c
  | ok t = Just t
  | otherwise = matchZeile ts iz c
  where
    ok = testZeile iz c

moveLSK :: Befehl -> LSK_Position -> LSK_Position
moveLSK (Drucke _) = id
moveLSK (Bewege_LSK_nach Rechts) = (+) 1
moveLSK (Bewege_LSK_nach Links) = \x -> x - 1

-- Spurfunktionen
type Spur = [GZustand]

spur :: GZustand -> Spur
spur gz
  | isJust $ matchZeile tafel iz (readRB rb pos) = gz : spur (transition gz)
  | otherwise = [gz]
  where
    (GZ tafel rb iz pos) = gz

zeige_zustand :: GZustand -> String
zeige_zustand (GZ _ rb iz pos)
  | isRBEmpty rb = "(IZ:" ++ show iz ++ ",LSK:" ++ show pos ++ ",B:"++ showRB rb ++")"
  | otherwise =
    "(IZ:"
      ++ show iz
      ++ ",LSK:"
      ++ show pos
      ++ ",B:"
      ++ showRB rb
      ++ ",Min:"
      ++ show lo
      ++ ",Max:"
      ++ show hi
      ++ ")"
  where
    (RB (B lo hi) _) = rb

isRBEmpty :: Rechenband -> Bool
isRBEmpty (RB U _) = True
isRBEmpty (RB _ _) = False

showRB :: Rechenband -> String
showRB (RB U _) = "unbeschrieben"
showRB (RB (B lo hi) band) = concatMap (showBZ . band) [lo .. hi]

showBZ :: Bandalphabet -> String
showBZ Blank = []
showBZ (Z z) = [z]

zeige_spur :: Spur -> String
zeige_spur [] = ""
zeige_spur (sz : spur) =
  zeige_zustand sz
    ++ concatMap ((" ->> " ++) . zeige_zustand) spur

-- Turingmaschinensimulatoreingabe
type Initiales_Rechenband = Rechenband

data Sim_Eingabe = SE Turingtafel Initiales_Rechenband

-- Turingmaschinensimulatorausgabe
type Finaler_interner_Zustand = Zustand

type Finale_LSK_Position = LSK_Position

type Finales_Rechenband = Rechenband

-- Abkuerzungen
type FIZ = Finaler_interner_Zustand

type FLSKP = Finale_LSK_Position

type FRB = Finales_Rechenband

-- Simulatorausgabe
data Sim_Ausgabe = SA FIZ FLSKP FRB

-- Turingmaschinensimulator, kurz Simulator
sim :: Sim_Eingabe -> Sim_Ausgabe
sim (SE tafel rb) = SA end_iz end_pos end_rb
  where
    gzs = spur (GZ tafel rb 0 0)
    (GZ _ end_rb end_iz end_pos) = last gzs

-- Ausgabe
instance Show Sim_Ausgabe where
  show (SA iz pos rb) =
    "IZ: " ++ show iz ++ " // LSKP: " ++ show pos ++ " // BI: " ++ bi rb
    where
      bi :: Rechenband -> String
      bi (RB U _) = "Leer"
      bi (RB (B lo hi) band) = show lo ++ ">" ++ showRB (RB (B lo hi) band) ++ "<" ++ show hi
