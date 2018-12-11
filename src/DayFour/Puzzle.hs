{-# LANGUAGE QuasiQuotes #-}

module DayFour.Puzzle where

import           Text.RawString.QQ (r)
import Text.Parser.Combinators (manyTill)
import           Text.Parser.Token (token, integer, brackets)
import Text.Trifecta.Parser (Parser)

example = [r|
[1518-03-11 00:45] wakes up
[1518-07-13 00:13] falls asleep
[1518-11-02 23:56] Guard #3463 begins shift
|]

row :: Parser [String]
row = do
  char '['
  time <- manyTill ']'
  time <- brackets
  time

partOne = do
  print "sup"
