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

data Date = Date Integer Integer Integer

row :: Parser Integer
row = do
  optional newline
  char '['
  year <- integer
  char '-'
  month <- integer
  char '-'
  day <- integer
  guardId <- try $ do
    string " Guard #"
    integer
  optional (manyTill anyChar newline)
  pure guardId

partOne = do
  print $ parseString (many row) mempty example
  print "sup"
