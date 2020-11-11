> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm!
3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!


Aufgabe A.1

> type Nat0   = Int
> type Ebene  = Nat0
> type Breite = Nat0
> type Text   = String
> data BBaum  = Blatt Text
>               | Knoten Text BBaum BBbaum deriving (Eq,Show)
>  data Auswertung = Ausw Breite [Ebene] deriving (Eq,Show)

> breitest :: BBaum -> Auswertung

breitest geht folgendermassen vor: ...


Aufgabe A.2

> tae :: BBaum -> Ebene -> Maybe [Text]

tae geht folgendermassen vor: ...


Aufgabe A.3

> data TBaum    = TB
>                 | TK TBaum TBaum TBaum deriving (Eq,Show)
> data Richtung = L | M | R deriving (Eq,Show)
> type Weg      = [Richtung]
> data TBaum'   = TB' Weg
>                 | TK' Weg TBaum' TBaum' TBaum' deriving (Eq,Show)

> awi :: TBaum -> TBaum'

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
