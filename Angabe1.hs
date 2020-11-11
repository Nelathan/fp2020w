module Angabe1 where
import Data.List

type Nat1 = Int

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Löschen Sie keine Deklarationen aus diesem Rahmenprogramm!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}

-- Aufgabe A.1
-- |The 'filtere' function sorts a List of Int accendingly then groups equal numbers and only uses groups of the right size n, then maps the groups back to the integer.
filtere :: Nat1 -> [Int] -> [Int]
filtere n l = [head x | x <- group (reverse (sort l)), n == length x]

-- Aufgabe A.2
-- |The 'kommt_vor' function tests a List of Int Tupels if x occurs in any of them.
kommt_vor :: Int -> [(Int, Int)] -> Bool
kommt_vor x l = any (==True) [x == a || x == b | (a,b) <- l]

-- Aufgabe A.3
-- |The 'aus' function sorts and groups a list of Int, it calculates the maximum length of all groups and then it replicates the value of each group times the maximum. Finally it concats all replications.
aus :: [Int] -> [Int]
aus l = concat [replicate (maximum (map (length) groups)) (head x) | x <- groups]
  where groups = group (sort l)


-- Aufgabe A.4
-- |The 'h' function determines the Hamming-distance for 2 equally long Strings by iterating over the length and checking if both chars are equal. Then the bools are mapped to 0 and 1 and summed up.
h :: String -> String -> Int
h s t
  | length s == length t = sum (map (fromEnum) [not (s!!n == t!!n) | n <- [0..(length s)-1]])
  | otherwise = -1
