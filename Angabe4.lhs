> module Angabe4 where
> import Data.List (elemIndices)

1. Vervollstaendigen Sie gemaess Angabentext!
2. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm!
3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!

> zipSum :: (Num a) => [a] -> [a] -> [a]
> zipSum (a : as) (b : bs) = a + b : zipSum as bs
> zipSum as [] = as
> zipSum [] bs = bs

> zipTree :: [[a]] -> [[a]] -> [[a]]
> zipTree (a : as) (b : bs) = (a ++ b) : zipTree as bs
> zipTree as [] = as
> zipTree [] bs = bs

Aufgabe A.1

> type Nat0   = Int
> type Ebene  = Nat0
> type Breite = Nat0
> type Text   = String
> data BBaum  = Blatt Text
>               | Knoten Text BBaum BBaum deriving (Eq,Show)
> data Auswertung = Ausw Breite [Ebene] deriving (Eq,Show)

> breitest :: BBaum -> Auswertung
> breitest baum = Ausw width $ elemIndices width ana
>   where
>     ana = analyzeBaum baum
>     width = maximum ana

> analyzeBaum :: BBaum -> [Nat0]
> analyzeBaum (Blatt _) = [1]
> analyzeBaum (Knoten _ l r) = 1 : zipSum al ar
>   where
>     al = analyzeBaum l
>     ar = analyzeBaum r

(Knoten "1" (Blatt "2") (Blatt "3"))

breitest geht folgendermassen vor: ...


Aufgabe A.2

> tae :: BBaum -> Ebene -> Maybe [Text]
> tae baum e
>   | e >= length es = Nothing
>   | otherwise = Just (es !! e)
>   where es = baumToText baum

> baumToText :: BBaum -> [[Text]]
> baumToText (Blatt t) = [[t]]
> baumToText (Knoten t l r) = [t]:zipTree (baumToText l) (baumToText r)

tae geht folgendermassen vor: ...


Aufgabe A.3

> data TBaum    = TB
>                 | TK TBaum TBaum TBaum deriving (Eq,Show)
> data Richtung = L | M | R deriving (Eq,Show)
> type Weg      = [Richtung]
> data TBaum'   = TB' Weg
>                 | TK' Weg TBaum' TBaum' TBaum' deriving (Eq,Show)

> awi :: TBaum -> TBaum'
> awi = undefined

awi geht folgendermassen vor: ...


Aufgabe A.4

> type Info a = [a]
> data Baum a = B (Info a)
>               | K (Baum a) (Info a) (Baum a)

> instance Show a => Show (Baum a) where

> instance Eq a => Eq (Baum a) where

> instance Ord a => Ord (Baum a) where

Die Instanzdeklarationen gehen folgendermassen vor: ...


Aufgabe A.5

> type UntereSchranke = Int
> type ObereSchranke  = Int
> data Intervall      = IV (UntereSchranke,ObereSchranke)
>                       | Leer
>                       | Ungueltig

> instance Show Intervall where
