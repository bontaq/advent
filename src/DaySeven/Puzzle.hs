{-# LANGUAGE QuasiQuotes #-}
-- |

module DaySeven.Puzzle where

import Text.RawString.QQ

import Control.Applicative
import Text.Parser.Token
import Text.Trifecta

import Debug.Trace
import Data.List
import Data.List (sortOn)
import Data.Maybe (fromJust)

input = [r|
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|]

newtype Card = Card Char
  deriving (Show, Eq)

ranks = "J23456789TQKA"

instance Ord Card where
  compare (Card a) (Card b) =
    let
      positionA = elemIndex a ranks
      positionB = elemIndex b ranks
    in case (positionA, positionB) of
      (Just indexA, Just indexB) -> compare indexA indexB
      _                          -> error "unknown value"

newtype Hand = Hand [Card]
  deriving (Show, Eq)

fiveOfAKind cards = length (nub cards) == 1

fourOfAKind cards = case group cards of
  [a, b] -> length a == 4 || length b == 4
  _      -> False

fullHouse cards = case group cards of
  [a, b] -> case (length a, length b) of
    (3, 2) -> True
    (2, 3) -> True
    _      -> False
  _      -> False

threeOfAKind cards =
  (== 1) . length . filter (== 3) $ length <$> group cards

twoPair cards =
  (== 2) . length . filter (== 2) $ length <$> group cards

onePair cards =
  (== 1) . length . filter (== 2) $ length <$> group cards

highCard cards = length (nub cards) == 5

handRank cards = fmap
  (\test -> test cards)
  [onePair, twoPair, threeOfAKind, fullHouse, fourOfAKind, fiveOfAKind]

strength cards =
  let
    sorted = sort cards
    handKinds = elemIndices True (handRank sorted)
  in if null handKinds then Nothing else Just (maximum handKinds)

smallCompare [] [] = EQ
smallCompare (a:cardsA) (b:cardsB) =
  if a == b then smallCompare cardsA cardsB
  else compare a b

instance Ord Hand where
  compare (Hand cardsA) (Hand cardsB) =
    case (strength cardsA, strength cardsB) of
      (Just a, Just b)   -> if a == b then smallCompare cardsA cardsB else compare a b
      (Just _, Nothing)  -> GT
      (Nothing, Just _)  -> LT
      (Nothing, Nothing) -> smallCompare cardsA cardsB

parseHand :: Parser (Hand, Integer)
parseHand = do
  optional newline
  hand <- some alphaNum
  whiteSpace
  bet <- integer

  pure (Hand $ Card <$> hand, bet)

pprint (Success cards) = mapM_ print cards

scoreHands :: [(Hand, Integer)] -> [Integer]
scoreHands hands =
  let ranked = zip [1..] hands
  in fmap (\(placement, (_, bet)) -> placement * bet) ranked

location = "./src/DaySeven/data.txt"

-- too low: 247513867

partOne = do
  raw <- readFile location
  let
    parsed = parseString (many parseHand) mempty raw
    ranked = sortOn fst <$> parsed
    scored = scoreHands <$> ranked
    total = sum <$> scored

  -- print result
  -- print real
  print parsed
  pprint ranked
  print scored
  print total


--
-- ty pos of fp discord, this helped me find my error

-- g x = fromJust $ elemIndex x "23456789TJQKA"
-- f = reverse . sort . map length . group . sort
-- comp (a, _) (b, _) = case compare (f a) (f b) of
--     EQ -> compare (g <$> a) (g <$> b)
--     x -> x
--
-- main = do
--   raw <- readFile location
--
--   let
--     ans :: [(String, Integer)]
--     ans = sortBy comp
--         . map ((\[a, b] -> (a, read b)) . words)
--         . lines $ raw
--
--   mapM_ print ans

--  print
--    . sum
--    . zipWith (*) [1..]
--    . map snd
--    . sortBy comp
--    . map ((\[a, b] -> (a, read b)) . words)
--    . lines $ raw
