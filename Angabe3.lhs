> module Angabe3 where
> import Data.List

1. Vervollstaendigen Sie gemaess Angabentext!
2. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!


> type UntereSchranke = Int
> type ObereSchranke  = Int
> data Intervall  = IV (UntereSchranke,ObereSchranke)
>                       | Leer
>                       | Ungueltig
>
> isEmpty :: Intervall -> Bool
> isEmpty Leer = True
> isEmpty i
>   | lBnd i > uBnd i = True
>   | otherwise = False
>
> isSingular :: Intervall -> Bool
> isSingular Leer = False
> isSingular i = lBnd i == uBnd i
>
> lBnd :: Intervall -> UntereSchranke
> lBnd (IV (u, _)) = u
>
> uBnd :: Intervall -> ObereSchranke
> uBnd (IV (_, o)) = o
>
> ivToRng :: Intervall -> [Int]
> ivToRng Leer = []
> ivToRng (IV (u,o)) = [u..o]

Aufgabe A.1

Die Instanzdeklaration fuer Show geht folgendermassen vor: ...

> instance Show Intervall where
>   show Ungueltig = "Kein Intervall"
>   show i
>     | isEmpty i = "<>"
>     | otherwise = "<" ++ show (lBnd i) ++ "," ++ show (uBnd i) ++ ">"


Aufgabe A.2

Die Instanzdeklaration fuer Eq geht folgendermassen vor: ...

> instance Eq Intervall where
>   Ungueltig == _ = error "Vergleich nicht moeglich"
>   _ == Ungueltig = error "Vergleich nicht moeglich"
>   a == b
>     | isEmpty a || isEmpty b = isEmpty a && isEmpty b
>     | otherwise = lBnd a == lBnd b && uBnd a == uBnd b


Aufgabe A.3

Die Instanzdeklaration fuer  Ord geht folgendermassen vor: ...

> instance Ord Intervall where
>   x > y
>     | x == y = False
>     | isEmpty x = False
>     | isEmpty y = True
>     | otherwise = lBnd x <= lBnd y && uBnd x >= uBnd y
>   x < y
>     | x == y = False
>     | isEmpty x = True
>     | isEmpty y = False
>     | otherwise = lBnd x >= lBnd y && uBnd x <= uBnd y
>   x >= y = x == y || x > y
>   x <= y = x == y || x < y


Aufgabe A.4

Die Instanzdeklaration fuer Num geht folgendermassen vor: ...

> instance Num Intervall where
>   Ungueltig + _ = error "Vergleich nicht moeglich"
>   _ + Ungueltig = error "Vergleich nicht moeglich"
>   a + b
>     | isEmpty a || isEmpty b = Leer
>     | otherwise = IV (lBnd a + lBnd b, uBnd a + uBnd b)
>
>   Ungueltig * _ = error "Vergleich nicht moeglich"
>   _ * Ungueltig = error "Vergleich nicht moeglich"
>   a * b
>     | i == [] = Leer
>     | otherwise = IV (minimum i, maximum i)
>     where
>       i = [xa * xb | xa <- ivToRng $ kanonisch a, xb <- ivToRng $ kanonisch b]
>
>   negate i = i * IV (-1,-1)
>
>   abs Ungueltig = error "Vergleich nicht moeglich"
>   abs i
>     | isEmpty i = Leer
>     | uBnd i < 0 = IV (-1 * uBnd i, -1 * lBnd i)
>     | lBnd i < 0 = IV (0, uBnd i)
>     | otherwise = i
>
>   signum i
>     | isEmpty i = 0
>     | i == abs i = 1
>     | i == abs (abs i) = -1
>
>   fromInteger x = IV (a,a) where
>     a = fromInteger x


Aufgabe A.5

Die Instanzdeklaration fuer Enum geht folgendermassen vor: ...

> instance Enum Intervall where
>   toEnum x = IV(x,x)
>   fromEnum Ungueltig = error "Operation nicht moeglich"
>   fromEnum i
>     | isSingular i = lBnd i
>     | otherwise = error "Operation nicht moeglich"


Aufgabe A.6

> class Kanonisch a where
>  kanonisch :: a -> a

Die Instanzdeklaration fuer Kanonisch geht folgendermassen vor: ...

> instance Kanonisch Intervall where
>   kanonisch Ungueltig = Ungueltig
>   kanonisch i
>     | isEmpty i = Leer
>     | otherwise = i


Aufgabe A.7

> class Element a where
>  is_elem :: Int -> a -> Maybe Bool

Die Instanzdeklaration fuer Element geht folgendermassen vor: ...

> instance Element Intervall where
>   is_elem _ Ungueltig = Nothing
>   is_elem x i
>     | isEmpty i = Just False
>     | x >= lBnd i && x <= uBnd i = Just True
>     | otherwise = Just False


Aufgabe A.8

> class Element a => Code a where
>  codiere :: [Int] -> a
>  decodiere :: a -> Maybe [Int]

Die Instanzdeklaration fuer Code geht folgendermassen vor: ...

> instance Code Intervall where
>   codiere a
>     | a == [] = Leer
>     | a == [minimum a .. maximum a] = IV (head a,last a)
>     | otherwise = Ungueltig
>   decodiere Ungueltig = Nothing
>   decodiere a
>     | isEmpty a = Just []
>     | otherwise = Just [lBnd a .. uBnd a]


Aufgabe A.9

> class (Ord a,Enum a) => ExTest a where
>   extrahiere :: Maybe [a] -> [a]
>   ist_aufsteigend :: [a] -> Bool
>   ist_lueckenlos :: [a] -> Bool
>   ist_laL_Element :: a -> Maybe [a] -> Bool

Die Instanzdeklaration fuer ExTest geht folgendermassen vor: ...

> instance ExTest Int where
>   extrahiere Nothing = error "Extraktion nicht moeglich"
>   extrahiere (Just a) = a
>   ist_aufsteigend a = a == sort a
>   ist_lueckenlos a
>     | a == [] = True
>     | otherwise = null [True | x <- [minimum a .. maximum a], not (elem x a)]
>   ist_laL_Element _ Nothing = False
>   ist_laL_Element a (Just b)
>     | b == [] = False
>     | ist_aufsteigend b && ist_lueckenlos b = a >= head b && a <= last b
>     | otherwise = False
