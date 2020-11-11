module Angabe2 where

type Nat0 = Integer

firstprimes :: [Integer]
firstprimes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

-- Aufgabe A.1

data IstEinsBinaerPrim = Ja | Nein deriving (Eq, Show)

toBinary :: Nat0 -> [Nat0]
toBinary 0 = [0]
toBinary 1 = [1]
toBinary n = (toBinary $ div n 2)++[mod n 2]

isPrime :: Nat0 -> Bool
isPrime n
  | n > 1 = primeFactors n == [n]
  | otherwise = False

primeFactors :: Nat0 -> [Nat0]
primeFactors n = [ i | i <- firstprimes++[101,103..n], mod n i == 0 ]

ist_einsbp :: Nat0 -> IstEinsBinaerPrim
ist_einsbp x
  | isPrime $ sum $ toBinary x = Ja
  | otherwise = Nein

-- Aufgabe A.2

type Von = Nat0
type Bis = Nat0
type VonBis = (Von, Bis)
type Anzahl0bps = Int
type Anzahl1bps = Int
type Anzahlen01bps = (Anzahl0bps, Anzahl1bps)

flipBinary :: [Nat0] -> [Nat0]
flipBinary [] = []
flipBinary (b:r) = (if b == 0 then 1 else 0):flipBinary r

anz01bps :: VonBis -> Anzahlen01bps
anz01bps (von, bis)
  |von<=bis = (
    sum [1 |b<-range, isPrime $ sum $ flipBinary b],
    sum [1 |b<-range, isPrime $ sum b]
  )
  |otherwise = (-1,-1)
  where range = [toBinary n | n<-[von..bis]]

{- anz01bps geht folgendermassen vor: ...
-}

-- Aufgabe A.3

type Wort = String

type Wortliste = [Wort]

liefere_woerter_in :: String -> Wortliste
liefere_woerter_in = lwi

lwi :: String -> Wortliste
lwi s = [w | w <- lwi1 s, length w > 0]

lwi1 :: String -> Wortliste
lwi1 "" = [[]]
lwi1 (c:s)
  |c==' '||c=='\t'||c=='\n' = []:a:b
  |otherwise = (c:a):b
  where (a : b) = lwi1 s

--lwi2 :: String -> (Wortliste, Bool)
--lwi2 "" = ([], False)
--lwi2 (c:s)
--  |c==' '||c=='\t'||c=='\n' = (a:b, False)
--  |addToA = ((c:a):b, True)
--  |otherwise = ([c]:a:b, True)
--  where (a:b, addToA) = lwi2 s
{- lwi geht folgendermassen vor: ...
-}

-- Aufgabe A.4

type Hammingabstand = Int

hM :: [String] -> Hammingabstand
--hM [] = -1
--hM (w:r)
--  |length r > 1 = min hw (hM r)
--  |length r == 1 = -1
--  |otherwise = 0
--  where hw = minimum [h w x | x<-r]
{- hM geht folgendermassen vor: ...
-}

hM l
  | length hs > 0 = minimum hs
  | otherwise = -1
  where
    hs = [h a b | (a,b) <- hammingTupels l]

hammingTupels :: [String] -> [(String, String)]
hammingTupels l = [(l!!a, l!!b) | a <- [0 .. length l - 1], b <- [1 .. length l - 1], a < b, l!!a /= l!!b]

h :: String -> String -> Int
h s t
  | s /= t && length s == length t = sum [if s !! n /= t !! n then 1 else 0 | n <- [0 .. length s - 1]]
  | otherwise = -1
