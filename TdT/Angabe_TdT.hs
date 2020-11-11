module Angabe_TdT where

-- Vervollstaendigen Sie gemaess Angabentext!

type Nat0 = Integer

-- Teil A: Zum Kennenlernen positiver Rueckmeldungen des Testsystems im Erfolgsfall

fakultaet :: Nat0 -> Nat0
fakultaet n
  | n >= 1 = n * fakultaet (n-1)
  | n == 0 = 1

fibonacci :: Nat0 -> Nat0
fibonacci n
  | n >= 2 = fibonacci (n-1) + fibonacci (n-2)
  | n == 1 = 1
  | n == 0 = 0

binom :: (Nat0,Nat0) -> Nat0
binom (n,k) = div (fakultaet n) (fakultaet k * fakultaet (n-k))

-- Teil B: Zum Kennenlernen negativer Rueckmeldungen des Testsystems im Misserfolgsfall

-- Inhaltlich fehlerhafte Implementierung
fak_fehlerhaft :: Nat0 -> Nat0
fak_fehlerhaft n
  | n < 0 = fak_fehlerhaft (-n)
  | n == 0 = 1
  | n <= 10 = n * fak_fehlerhaft (n-1)
  | n <= 15 = n + fak_fehlerhaft (n-1)
  | True = fak_fehlerhaft 42

-- Inhaltl. fehlerhaft, zusatzl. fehlt die vom Testsystem geforderte Typsignatur
-- fib_fehlerhaft :: Nat0 -> Nat0
fib_fehlerhaft 0 = 0
fib_fehlerhaft 1 = 1
fib_fehlerhaft n = fib_fehlerhaft (n-2) * fib_fehlerhaft (n-1)

binom_fehlerhaft :: (Nat0,Nat0) -> Nat0
binom_fehlerhaft (n,k) = div (fakultaet n) (fakultaet k * fak_fehlerhaft (n-k))

-- Inhaltlich korrekte, aber Zeituberschreitungen erzwingende Implementierung
binom_langsam :: (Nat0,Nat0) -> Nat0
binom_langsam (n,k)
  | k == 0 || n == k = 1
  | True = binom_langsam (n-1,k-1) + binom_langsam (n-1,k)
