{-# LANGUAGE QuasiQuotes #-}

module DayFour.Puzzle where

import           Text.RawString.QQ (r)
import Text.Parser.Combinators
import Text.Parser.Char
import           Text.Parser.Token
import Text.Trifecta.Parser (Parser, parseString, parseFromFile)
import Control.Applicative
import Data.List

example = [r|
[1518-03-11 00:45] wakes up
[1518-07-13 00:13] falls asleep
[1518-11-02 23:56] Guard #3463 begins shift
|]

type Id = Integer

data Date = Date Integer Integer Integer
          deriving (Show, Eq)
data Time = Time Integer Integer
          deriving (Show, Eq)
data DateTime = DateTime Date Time
              deriving (Show, Eq)

instance Ord Date where
  compare first@(Date a b c) second@(Date a' b' c') =
    case compare a a' of
      EQ -> case compare b b' of
        EQ -> compare c c'
        _ -> compare b b'
      _ -> compare a a'

instance Ord Time where
  compare first@(Time a b) second@(Time a' b') =
    case compare a a' of
      EQ -> compare b b'
      _ -> compare a a'

instance Ord DateTime where
  compare (DateTime d t) (DateTime d' t')
    | d == d' = compare t t'
    | otherwise = compare d d'

instance Ord Row where
  compare (Row dt _) (Row dt' _) = compare dt dt'

data Action = Sleep | WakeUp | Guard Id
             deriving (Show, Eq)

guard :: Parser Action
guard = do
  string "Guard #"
  integer >>= pure . Guard

wake :: Parser Action
wake = do
  string "wakes"
  pure WakeUp

sleep :: Parser Action
sleep = do
  string "falls"
  pure Sleep

data Row = Row DateTime Action
         deriving (Show, Eq)

dateParse :: Parser Date
dateParse = do
  year <- integer
  char '-'
  month <- integer
  char '-'
  day <- integer
  pure $ Date year month day

timeParse :: Parser Time
timeParse = do
  hour <- integer
  char ':'
  minute <- integer
  pure $ Time hour minute

row :: Parser Row
row = do
  optional newline

  char '['
  date <- dateParse
  time <- timeParse
  string "] "

  action <- guard <|> wake <|> sleep

  optional (manyTill anyChar newline)

  pure $ Row (DateTime date time) action

labelRows :: Integer -> [Row] -> [(Id, Row)]
labelRows _ [] = []
labelRows activeGuard (r:rs) =
  let (Row date action) = r
  in case action of
    (Guard id) -> (id, r) : (labelRows id rs)
    _          -> (activeGuard, r) : (labelRows activeGuard rs)

sleepRanges :: Integer -> [(Id, Row)] -> [Integer]
sleepRanges lastSleep [] = []
sleepRanges lastSleep (r:rs) =
  let (_, (Row date action)) = r
      (DateTime _ t) = date
      (Time _ m) = t
  in case action of
    Sleep -> sleepRanges m rs
    WakeUp -> [lastSleep..(m - 1)] ++ sleepRanges 0 rs
    _ -> sleepRanges 0 rs

totalSleep :: [(Id, Row)] -> Int
totalSleep = length . (sleepRanges 0)

favoriteSleep :: [(Id, Row)] -> [[Integer]]
favoriteSleep rows =
  sortBy (\a b -> compare (length a) (length b))
  $ group . sort
  $ sleepRanges 0 rows

partOne :: IO ()
partOne = do
  res <- parseFromFile (many row) "./src/DayFour/Data.txt"
  let ordered = fmap sort res
      labeled = fmap (labelRows 0) ordered
      grouped = fmap (groupBy (\a b -> fst a == fst b))  labeled

      totalSleeps = fmap (map (\(rows) -> (totalSleep rows, rows))) grouped
      sleepiest = fmap
        (head . sortBy (\a b -> compare (fst b) (fst a))) totalSleeps
      favorite = fmap (\(_, rows) -> favoriteSleep rows) sleepiest

  -- print totalSleeps
  print sleepiest
  print favorite
  -- mapM (print . mappend "\n" . show) grouped
  -- totalSleeps >>= pure . printRows
  -- pure $ printRows totalSleeps
  -- print test
  -- fmap print grouped
  -- print $ parseString (many row) mempty example
  print "sup"
