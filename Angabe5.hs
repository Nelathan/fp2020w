module Angabe5 where

import Data.List
import Data.Maybe
import Debug.Trace

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Löschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}

type Nat0 = Int

type Nat1 = Int

type Vorname = String

type Nachname = String

data Partei = ABC | DEF | GHI | JKL | MNO deriving (Eq, Show)

data Wahlwerber = WW Vorname Nachname Partei deriving (Eq, Show)

type Wahlvorschlag = [Wahlwerber]

type Wahlvorschlagsplatz = Nat1

type Wahlsieger = (Wahlwerber, Wahlvorschlagsplatz)

type Stimmzettel = [Wahlvorschlagsplatz]

type Wahl = [Stimmzettel]

type Gueltig = [Stimmzettel]

type Ungueltig = [Stimmzettel]

type Platz_1_Stimmen = Nat0


-- Aufgabe A.1

ist_gueltiger_Wahlvorschlag :: Wahlvorschlag -> Bool
ist_gueltiger_Wahlvorschlag wv = not $ null wv

-- Aufgabe A.2

ist_gueltiger_Stimmzettel :: Wahlvorschlag -> Stimmzettel -> Bool
ist_gueltiger_Stimmzettel wv sz = wvOK && projectable && noDupes
  where
    wvOK = ist_gueltiger_Wahlvorschlag wv
    projectable = all (length wv >=) sz && all (0 <) sz
    noDupes = sz == nub sz

-- Aufgabe A.3

trenne_Stimmzettel :: Wahlvorschlag -> Wahl -> (Gueltig, Ungueltig)
trenne_Stimmzettel _ [] = ([], [])
trenne_Stimmzettel wv w = partition (ist_gueltiger_Stimmzettel wv) w

-- Aufgabe A.4
auszaehlen :: Wahlvorschlag -> Wahl -> Maybe [Platz_1_Stimmen]
auszaehlen wv w
  | not $ ist_gueltiger_Wahlvorschlag wv = Nothing
  | not . all (ist_gueltiger_Stimmzettel wv) $ w = Nothing
  | otherwise = Just [bagsize c | c <- [1..length wv]]
  where
    bags = map (\bag -> (head bag, length bag)) $ group $ sort $ map head $ filter (not . null) w
    bagsize c = maybe 0 snd $ find ((c ==) . fst) bags

-- Aufgabe A.5

wahlsieger :: Wahlvorschlag -> Maybe [Platz_1_Stimmen] -> Maybe Wahlsieger
{- wahlsieger geht folgendermassen vor: ...
-}
wahlsieger _ Nothing = Nothing
wahlsieger wv (Just no1)
  | not $ ist_gueltiger_Wahlvorschlag wv = Nothing
  | length no1 /= length wv = Nothing
  | otherwise = winner >>= (\winner -> Just (wv!!winner, winner + 1))
  where
    winner = findIndex (minVotes <) no1
    minVotes = div (sum no1) 2


-- Aufgabe A.6
reduceStimmzettel :: [Nat0] -> Stimmzettel -> Stimmzettel
reduceStimmzettel [] sz = sz
reduceStimmzettel cs sz = [s | s <- sz, s `notElem` cs]

ausscheiden :: Wahl -> [Platz_1_Stimmen] -> Wahl
ausscheiden w no1
  | null votes = w
  | otherwise = map (reduceStimmzettel toRemove) w
  where
    votes = filter (0 /=) no1
    lowest = (>=) $ minimum votes
    toRemove = map fst $ filter (lowest . snd) $ zip [1 ..] no1

-- Aufgabe A.7

data Wahlausgang
  = Ungueltiger_Wahlvorschlag
  | Keine_gueltigen_Stimmen
  | Gewaehlt_ist Wahlwerber
  | Kein_Wahlsieger_Wahlwiederholung
  deriving (Eq, Show)

test :: Show a => a -> a
test a = trace (show a) a

wahlausgang :: Wahlvorschlag -> Wahl -> Wahlausgang
wahlausgang wv w
  | not (ist_gueltiger_Wahlvorschlag wv) = Ungueltiger_Wahlvorschlag
  | null gueltig = Keine_gueltigen_Stimmen
  | otherwise = stichwahl wv gueltig
  where
    gueltig = fst $ trenne_Stimmzettel wv w

stichwahl :: Wahlvorschlag -> Wahl -> Wahlausgang
stichwahl _ [] = Kein_Wahlsieger_Wahlwiederholung
stichwahl wv w = maybe alternative (Gewaehlt_ist . fst) winner
  where
    ausz = auszaehlen wv w
    winner = wahlsieger wv ausz
    alternative = maybe Kein_Wahlsieger_Wahlwiederholung subwahl ausz
    subwahl no1 = stichwahl wv $ filter (not . null) $ ausscheiden w no1


-- Aufgabe A.8
data Groesste_Wahlverlierer
  = GWV [Partei]
  | Keine
  | Analyse_nicht_moeglich
  deriving (Eq, Show)

wahlanalyse :: Wahlvorschlag -> Wahl -> Groesste_Wahlverlierer
wahlanalyse wv w
  | not (ist_gueltiger_Wahlvorschlag wv) = Analyse_nicht_moeglich
  | null gueltig = Analyse_nicht_moeglich
  | otherwise = knockout wv Nothing gueltig
  where
    gueltig = fst $ trenne_Stimmzettel wv w

knockout :: Wahlvorschlag -> Maybe [Partei] -> Wahl -> Groesste_Wahlverlierer
knockout wv p w
  | (not.null) w && isNothing winner = knockout wv loser restVotes
  | otherwise = declareLoser loser
  where
    ausz = auszaehlen wv w
    loser = if isNothing p then lostParties wv w else p
    winner = wahlsieger wv ausz
    restVotes = filter (not.null) $ ausscheiden w (fromMaybe [] ausz)

lostParties :: Wahlvorschlag -> Wahl -> Maybe [Partei]
lostParties _ [] = Nothing
lostParties wv w
  | null lost2 = Nothing
  | otherwise = Just lost2
  where
    p = (\ps -> trace ("left "++show ps) ps ) $ wahlToParties wv $ test w
    pall = nub $ map partyOfWW wv
    lost2 = pall \\ p
    --lost = nub $ map (partyOfWW.(wv!!).fst) $ filter ((0==).snd) $ zip [0..] ausz

wahlToParties :: Wahlvorschlag -> Wahl -> [Partei]
wahlToParties wv w = nub $ map (partyOfWW.(wv !!).(-1+)) $ foldl union [] w

declareLoser :: Maybe [Partei] -> Groesste_Wahlverlierer
declareLoser Nothing = Keine
declareLoser (Just ps) = GWV ps

partyOfWW :: Wahlwerber -> Partei
partyOfWW (WW _ _ p) = p
