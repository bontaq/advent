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

partOne :: IO ()
partOne = do
  res <- parseFromFile (many row) "./src/DayFour/Data.txt"
  print $ fmap sort res
  -- print $ parseString (many row) mempty example
  print "sup"
