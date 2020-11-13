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
> awi = treeAnnotation []

> treeAnnotation :: [Richtung] -> TBaum -> TBaum'
> treeAnnotation path TB = TB' path
> treeAnnotation path (TK l m r) = TK' path (treeAnnotation (path++[L]) l) (treeAnnotation (path++[M]) m) (treeAnnotation (path++[R]) r)

awi geht folgendermassen vor: ...


Aufgabe A.4

> type Info a = [a]
> data Baum a = B (Info a)
>               | K (Baum a) (Info a) (Baum a)

> instance Show a => Show (Baum a) where
>   show (B a) = "<"++show a++">"
>   show (K l a r) = "<Wurzel "++show a++" "++show l++" "++show r++">"

> instance Eq a => Eq (Baum a) where
>   (B a) == (B b) = a == b
>   (K la ia ra) == (K lb ib rb) = ia == ib && la == lb && ra == rb
>   _ == _ = False

> eab :: Baum a -> Baum a -> Ordering
> eab (B _) (B _) = EQ
> eab K{} (B _) = GT
> eab (B _) K{} = LT
> eab (K la _ ra) (K lb _ rb)
>   | cl == EQ && cr == EQ = EQ
>   | (cl == GT || cr == GT) && not (cl == LT || cr == LT) = GT
>   | otherwise = LT
>   where
>     cl = eab la lb
>     cr = eab ra rb

echte Anfangsliste

> eal :: (Ord a) => [a] -> [a] -> Bool
> eal (_:_) [] = True
> eal (a:as) (b:bs) = a == b && eal as bs
> eal _ _ = False

Anfangsbaum hat Anfangslisten

> ealt :: (Ord a) => Baum a -> Baum a -> Bool
> ealt (B a) (B b) = eal a b
> ealt (K _ a _) (B b) = eal a b
> ealt (K la ia ra) (K lb ib rb) = eal ia ib && ealt la lb && ealt ra rb
> ealt _ _ = False

> instance Ord a => Ord (Baum a) where
>   a > b = eab a b == GT && ealt a b
>   a < b = b > a
>   a >= b = a == b || a > b
>   a <= b = a == b || a < b

Die Instanzdeklarationen gehen folgendermassen vor: ...


Aufgabe A.5



> type UntereSchranke = Int
> type ObereSchranke  = Int
> data Intervall      = IV (UntereSchranke,ObereSchranke)
>                       | Leer
>                       | Ungueltig

> isEmpty :: Intervall -> Bool
> isEmpty Leer = True
> isEmpty i
>   | lBnd i > uBnd i = True
>   | otherwise = False

> lBnd :: Intervall -> UntereSchranke
> lBnd (IV (u, _)) = u
>
> uBnd :: Intervall -> ObereSchranke
> uBnd (IV (_, o)) = o

> instance Show Intervall where
>   show Ungueltig = "Kein Intervall"
>   show i
>     | isEmpty i = "<>"
>     | otherwise = "<" ++ show (lBnd i) ++ "," ++ show (uBnd i) ++ ">"
