module Angabe3 where
import Data.List
--1. Vervollstaendigen Sie gemaess Angabentext!
--2. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
--3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!


type UntereSchranke = Int
type ObereSchranke  = Int
data Intervall  = IV (UntereSchranke,ObereSchranke)
                    | Leer
                    | Ungueltig


-- Aufgabe A.1

--Die Instanzdeklaration fuer Show geht folgendermassen vor: ...

isEmpty :: Intervall -> Bool
isEmpty Leer = True
isEmpty i
  | lBnd i > uBnd i = True
  | otherwise = False

isSingular :: Intervall -> Bool
isSingular Leer = False
isSingular i = lBnd i == uBnd i

lBnd :: Intervall -> UntereSchranke
lBnd (IV (u, _)) = u

uBnd :: Intervall -> ObereSchranke
uBnd (IV (_, o)) = o

ivToRng :: Intervall -> [Int]
ivToRng Leer = []
ivToRng (IV (u,o)) = [u..o]

instance Show Intervall where
  show Ungueltig = "Kein Intervall"
  show i
    | isEmpty i = "<>"
    | isSingular i = show (lBnd i)
    | otherwise = "<" ++ show (lBnd i) ++ "," ++ show (uBnd i) ++ ">"

instance Eq Intervall where
  Ungueltig == _ = error "Vergleich nicht moeglich"
  _ == Ungueltig = error "Vergleich nicht moeglich"
  a == b
    | isEmpty a || isEmpty b = isEmpty a && isEmpty b
    | otherwise = lBnd a == lBnd b && uBnd a == uBnd b

instance Ord Intervall where
  compare a b
    | a == b = EQ
    | isEmpty a = LT
    | isEmpty b = GT
    | lBnd a >= lBnd b && uBnd a <= uBnd b = LT
    | lBnd a <= lBnd b && uBnd a >= uBnd b = GT
    | otherwise = error "Vergleich nicht moeglich"

instance Num Intervall where
  Ungueltig + _ = error "Vergleich nicht moeglich"
  _ + Ungueltig = error "Vergleich nicht moeglich"
  a + b
    | isEmpty a || isEmpty b = Leer
    | otherwise = IV (lBnd a + lBnd b, uBnd a + uBnd b)
  a * b
    | i == [] = Leer
    | otherwise = IV (minimum i, maximum i)
    where
      i = [xa * xb | xa <- ivToRng $ kanonisch a, xb <- ivToRng $ kanonisch b]
  negate i = i * IV (-1,-1)
  fromInteger x = IV (a,a) where
    a = fromInteger x
  abs i
    | isEmpty i = Leer
    | uBnd i < 0 = IV (-1 * uBnd i, -1 * lBnd i)
    | lBnd i < 0 = IV (0, uBnd i)
    | otherwise = i
  signum i
    | isEmpty i = 0
    | i == abs i = 1
    | i == abs (abs i) = -1

instance Enum Intervall where
  toEnum x = IV(x,x)
  fromEnum Ungueltig = error "Operation nicht moeglich"
  fromEnum i
    | isSingular i = lBnd i
    | otherwise = error "Operation nicht moeglich"

class Kanonisch a where
  kanonisch :: a -> a

instance Kanonisch Intervall where
  kanonisch Ungueltig = Ungueltig
  kanonisch i
    | isEmpty i = Leer
    | otherwise = i

class Element a where
  is_elem :: Int -> a -> Maybe Bool

instance Element Intervall where
  is_elem _ Ungueltig = Nothing
  is_elem x i
    | isEmpty i = Just False
    | x >= lBnd i && x <= uBnd i = Just True
    | otherwise = Just False

class Element a => Code a where
  codiere   :: [Int] -> a
  decodiere :: a -> Maybe [Int]

instance Code Intervall where
  codiere a
    | a == [] = Leer
    | a == [minimum a .. maximum a] = IV (head a,last a)
    | otherwise = Ungueltig
  decodiere Ungueltig = Nothing
  decodiere a
    | isEmpty a = Just []
    | otherwise = Just [lBnd a .. uBnd a]

class (Ord a,Enum a) => ExTest a where
  extrahiere :: Maybe [a] -> [a]
  ist_aufsteigend :: [a] -> Bool
  ist_lueckenlos :: [a] -> Bool
  ist_laL_Element :: a -> Maybe [a] -> Bool

instance ExTest Int where
  extrahiere Nothing = error "Extraktion nicht moeglich"
  extrahiere (Just a) = a
  ist_aufsteigend a = a == sort a
  ist_lueckenlos a
    | a == [] = True
    | otherwise = null [True | x <- [minimum a .. maximum a], not (elem x a)]
  ist_laL_Element _ Nothing = False
  ist_laL_Element a (Just b)
    | b == [] = False
    | ist_aufsteigend b && ist_lueckenlos b = a >= head b && a <= last b
    | otherwise = False
