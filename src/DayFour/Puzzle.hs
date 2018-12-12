{-# LANGUAGE QuasiQuotes #-}

module DayFour.Puzzle where

import           Text.RawString.QQ (r)
import Text.Parser.Combinators
import Text.Parser.Char
import           Text.Parser.Token
import Text.Trifecta.Parser (Parser, parseString)
import Control.Applicative

example = [r|
[1518-03-11 00:45] wakes up
[1518-07-13 00:13] falls asleep
[1518-11-02 23:56] Guard #3463 begins shift
|]

type Id = Integer

data Date = Date Integer Integer Integer
          deriving Show
data Time = Time Integer Integer
          deriving Show
data DateTime = DateTime Date Time
              deriving Show

data Action = Sleep | WakeUp | Guard Id
             deriving Show

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
         deriving Show

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

partOne = do
  print $ parseString (many row) mempty example
  print "sup"
