module Angabe6 where

import Data.List

-- Aufgabe A.1

gen_sort :: (a -> a -> Bool) -> [a] -> [a]
gen_sort cmp = sortBy order
  where
    order a b
      | cmp a b = LT
      | otherwise = GT

gen_insert :: (a -> a -> Bool) -> a -> [a] -> [a]
gen_insert _ a [] = [a]
gen_insert cmp x (y : ys)
  | cmp x y = x : y : ys
  | otherwise = y : gen_insert cmp x ys

-- Aufgabe A.3

auf_ord :: Ord a => [a] -> [a]
auf_ord = gen_sort (<)

ab_ord :: Ord a => [a] -> [a]
ab_ord = gen_sort (>)

-- Aufgabe A.5

type UntereSchranke = Int

type ObereSchranke = Int

data Intervall
  = IV (UntereSchranke, ObereSchranke)
  | Leer
  | Ungueltig

type Text = String

data BBaum
  = Blatt Text
  | Knoten Text BBaum BBaum
  deriving (Eq, Ord, Show)

data TBaum
  = TB
  | TK TBaum TBaum TBaum
  deriving (Eq, Ord, Show)

type Info a = [a]

data Baum a
  = B (Info a)
  | K (Baum a) (Info a) (Baum a)

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
ivToRng (IV (u, o)) = [u .. o]

instance Show Intervall where
  show Ungueltig = "Kein Intervall"
  show i
    | isEmpty i = "<>"
    | otherwise = "<" ++ show (lBnd i) ++ "," ++ show (uBnd i) ++ ">"

instance Eq Intervall where
  Ungueltig == _ = error "Vergleich nicht moeglich"
  _ == Ungueltig = error "Vergleich nicht moeglich"
  a == b
    | isEmpty a || isEmpty b = isEmpty a && isEmpty b
    | otherwise = lBnd a == lBnd b && uBnd a == uBnd b

instance Ord Intervall where
  x > y
    | x == y = False
    | isEmpty x = False
    | isEmpty y = True
    | otherwise = lBnd x <= lBnd y && uBnd x >= uBnd y
  x < y
    | x == y = False
    | isEmpty x = True
    | isEmpty y = False
    | otherwise = lBnd x >= lBnd y && uBnd x <= uBnd y
  x >= y = x == y || x > y
  x <= y = x == y || x < y

instance Show a => Show (Baum a) where
  show (B a) = "<" ++ show a ++ ">"
  show (K l a r) = "<Wurzel " ++ show a ++ " " ++ show l ++ " " ++ show r ++ ">"

instance Eq a => Eq (Baum a) where
  (B a) == (B b) = a == b
  (K la ia ra) == (K lb ib rb) = ia == ib && la == lb && ra == rb
  _ == _ = False

eab :: Baum a -> Baum a -> Ordering
eab (B _) (B _) = EQ
eab K {} (B _) = GT
eab (B _) K {} = LT
eab (K la _ ra) (K lb _ rb)
  | cl == EQ && cr == EQ = EQ
  | (cl == GT || cr == GT) && not (cl == LT || cr == LT) = GT
  | otherwise = LT
  where
    cl = eab la lb
    cr = eab ra rb

-- echte Anfangsliste
eal :: (Ord a) => [a] -> [a] -> Bool
eal (_ : _) [] = True
eal (a : as) (b : bs) = a == b && eal as bs
eal _ _ = False

-- Anfangsbaum hat Anfangslisten
ealt :: (Ord a) => Baum a -> Baum a -> Bool
ealt (B a) (B b) = eal a b
ealt (K _ a _) (B b) = eal a b
ealt (K la ia ra) (K lb ib rb) = eal ia ib && ealt la lb && ealt ra rb
ealt _ _ = False

instance Ord a => Ord (Baum a) where
  a > b = eab a b == GT && ealt a b
  a < b = b > a
  a >= b = a == b || a > b
  a <= b = a == b || a < b

-- Aufgabe A.8
len_sort :: (Int -> Int -> Bool) -> [[a]] -> [[a]]
len_sort cmp = gen_sort (\a b -> cmp (length a) (length b))

auf_lst :: [[a]] -> [[a]]
auf_lst = len_sort (<)

ab_lst :: [[a]] -> [[a]]
ab_lst = len_sort (>)

-- -- Aufgabe A.10

fun_sort :: (Int -> Int -> Bool) -> [Int -> Int] -> [Int -> Int]
fun_sort cmp = gen_sort (\a b -> cmp (a 0) (b 0))

auf_fun :: [Int -> Int] -> [Int -> Int]
auf_fun = fun_sort (<)

ab_fun :: [Int -> Int] -> [Int -> Int]
ab_fun = fun_sort (>)

-- -- Aufgabe A.11
onfun_sort :: (Ord a, Num a) => (a -> a -> Bool) -> [a -> a] -> [a -> a]
onfun_sort cmp = gen_sort (\a b -> cmp (a 0) (b 0))

auf_onfun :: (Ord a, Num a) => [a -> a] -> [a -> a]
auf_onfun = onfun_sort (<)

ab_onfun :: (Ord a, Num a) => [a -> a] -> [a -> a]
ab_onfun = onfun_sort (>)

-- Aufgabe A.13

type Nat1 = Int

type Name = String

type Alter = Nat1

data Geschlecht = M | F | D deriving (Eq, Ord, Show)

type Gehalt = Nat1

type PersNummer = Int

data Hersteller
  = Alcatel
  | Apple
  | Huawai
  | LG
  | Motorola
  | Nokia
  | Samsung
  deriving (Eq, Ord, Show)

type Hat_Smartphone_von = Maybe Hersteller

data Person
  = P
      PersNummer
      Name
      Alter
      Geschlecht
      Gehalt
      Hat_Smartphone_von
  deriving (Eq, Show)

type Datenbank = [Person]

type Nutzungssicht = Datenbank

getPnr :: Person -> PersNummer
getPnr (P x _ _ _ _ _) = x

getName :: Person -> Name
getName (P _ x _ _ _ _) = x

getAlter :: Person -> Alter
getAlter (P _ _ x _ _ _) = x

getGeschlecht :: Person -> Geschlecht
getGeschlecht (P _ _ _ x _ _) = x

getGehalt :: Person -> Gehalt
getGehalt (P _ _ _ _ x _) = x

getPhone :: Person -> Hat_Smartphone_von
getPhone (P _ _ _ _ _ x) = x

normalsicht :: Datenbank -> Nutzungssicht
normalsicht = gen_sort (\a b -> getName a < getName b)

anlageberatungssicht :: Datenbank -> Nutzungssicht
anlageberatungssicht = gen_sort (\a b -> getGehalt a > getGehalt b)

personalabteilungssicht :: Datenbank -> Nutzungssicht
personalabteilungssicht = gen_sort cmp
  where
    cmp a b
      | getGeschlecht a /= getGeschlecht b = getGeschlecht a > getGeschlecht b
      | otherwise = getAlter a <= getAlter b

sozialforschungssicht :: Datenbank -> Nutzungssicht
sozialforschungssicht = gen_sort cmp
  where
    cmp a b
      | getPhone a /= getPhone b = cmpPhone (getPhone a) (getPhone b)
      | otherwise = getGehalt a <= getGehalt b
    cmpPhone Nothing _ = False
    cmpPhone _ Nothing = True
    cmpPhone a b = a < b

integritaetssicht :: Datenbank -> Nutzungssicht
integritaetssicht =
  concat
    . gen_sort cmp
    . groupBy (\a b -> getPnr a == getPnr b)
    . gen_sort (\a b -> getPnr a < getPnr b)
  where
    cmp a b
      | length a /= length b = length a > length b
      | otherwise = getPnr (head a) < getPnr (head b)

auch_im_chaos_ist_ordnung_sicht :: Datenbank -> Nutzungssicht
auch_im_chaos_ist_ordnung_sicht = gen_sort cmp
  where
    cmp a b = mapping a < mapping b
    mapping = head . (++ " ") . getName
