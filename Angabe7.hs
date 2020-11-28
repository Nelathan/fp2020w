module Angabe7 where
import Data.List
import Data.Maybe

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
      | zeichen == Blank && (range == U || lo == hi) = U
      | zeichen == Blank && feld == lo = B (scan [lo + 1 .. hi]) hi
      | zeichen == Blank && feld == hi = B lo (scan [hi -1 .. lo])
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


-- Komfortfunktion zur Vereinfachung der Turingmaschineneingabe
wandle_in_rb :: [Zeichenvorrat] -> Rechenband
wandle_in_rb = write leer_rb 0
  where
    write :: Rechenband -> Bandfeld -> [Zeichenvorrat] -> Rechenband
    write rb _ [] = rb
    write rb m (c:cs) = write (akt_rechenband rb m (Z c)) (m+1) cs

getIZ :: Zeile -> Interner_Zustand
getIZ (iz, _, _, _) = iz

getLSKZ :: Zeile -> Bandalphabet
getLSKZ (_, lskz, _, _) = lskz

getBefehl :: Zeile -> Befehl
getBefehl (_, _, b, _) = b

getIFZ :: Zeile -> Interner_Folgezustand
getIFZ (_, _, _, ifz) = ifz

eqZeile :: Zeile -> Zeile -> Bool
eqZeile (iz, lskz, _, _) (iz2, lskz2, _, _) = iz == iz2 && lskz == lskz2

isMove :: Befehl -> Bool
isMove (Bewege_LSK_nach _) = True

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
  | isMove befehl = GZ tafel rb ifz (readRB rb pos)
  | otherwise = GZ tafel (akt_rechenband rb iz druckz) ifz druckz
  where
    next = matchZeile tafel iz lskz
    befehl = getBefehl $ fromJust next
    ifz = getIFZ $ fromJust next
    druckz = (\(Drucke z) -> z) befehl


matchZeile :: Turingtafel -> Interner_Zustand -> LSKZ -> Maybe Zeile
matchZeile [] _ _ = Nothing
matchZeile (t:ts) iz lskz
  | eqZeile t (iz, lskz, undefined, undefined) = Just t
  | otherwise = matchZeile ts iz lskz

moveLSK :: Befehl -> IZ -> IZ
moveLSK (Drucke _) = id
moveLSK (Bewege_LSK_nach Rechts) = (+) 1
moveLSK (Bewege_LSK_nach Links) = (-) 1

-- Spurfunktionen
type Spur = [GZustand]

spur :: GZustand -> Spur
spur = undefined

zeige_zustand :: GZustand -> String
zeige_zustand = undefined

zeige_spur :: Spur -> String
zeige_spur = undefined

-- Turingmaschinensimulator, kurz Simulator
sim :: Sim_Eingabe -> Sim_Ausgabe
sim = undefined

-- Ausgabe
instance Show Sim_Ausgabe where
  show = undefined
