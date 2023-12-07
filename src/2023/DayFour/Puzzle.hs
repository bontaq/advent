{-# LANGUAGE QuasiQuotes #-}
-- |

module DayFour.Puzzle where

import Text.RawString.QQ

import Control.Applicative
import Text.Parser.Token
import Text.Trifecta
import Text.Trifecta (manyTill)
import Control.Monad (liftM)
import GHC.IO (liftIO)
import Data.List (intersect, sortBy)
import GHC.Arr (newSTArray)


test = [r|
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|]

data Card = Card
  { cardId :: Integer
  , winning :: [Integer]
  , held :: [Integer]
  } deriving (Show)

nums :: Parser Integer
nums = do
  optional (some space)
  num <- integer
  pure num

parseCard :: Parser Card
parseCard = do
  optional newline

  string "Card"
  some space
  cardId <- integer
  char ':'

  winning <- manyTill nums (char '|')
  held <- some nums

  pure $ Card cardId winning held

determineScore :: Card -> Integer
determineScore (Card _ win held) =
  let wins = win `intersect` held
  in
    if null wins
    then 0
    else 2 ^ (toInteger $ length wins - 1)

location = "./src/2023/DayFour/data.txt"

partOne = do
  raw <- readFile location

  let
    cards = parseString (many parseCard) mempty raw
    scores = fmap determineScore <$> cards
    total = sum <$> scores

  print cards
  print scores
  print total
  pure ()

newScore (Card cardId win held) = length $ win `intersect` held

playGame :: Bool -> [Card] -> Int
playGame original [] = 0
playGame original (c:cs) =
  let
    score = newScore c
    decks =
      if score > 0
      then fmap (\toRemove -> drop toRemove cs) [0..score - 1]
      else []
  in
    1
    + (if original then playGame True cs else 0)
    + (sum $ fmap (playGame False) decks)

playGame' :: Bool -> [Card] -> [Card]
playGame' original [] = []
playGame' original (c:cs) =
  let
    score = newScore c
    decks =
      if score > 0
      then fmap (\toRemove -> drop toRemove cs) [0..score - 1]
      else []
  in
    [c]
    <> (if original then playGame' True cs else [])
    <> (concat $ fmap (playGame' False) decks)

printResult :: Result [Card] -> IO ()
printResult (Success cards) =
  mapM_ print (sortBy (\(Card gameId _ _) (Card gameId' _ _) -> gameId `compare` gameId') cards)

checkScores = do
  let
    cards = parseString (many parseCard) mempty test
    scores = fmap newScore <$> cards

  print scores

partTwo = do
  raw <- readFile location

  let
    cards = parseString (many parseCard) mempty test
    round = playGame True <$> cards

  print round
  -- printResult round

  pure ()
