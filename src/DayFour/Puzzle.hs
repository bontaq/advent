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

row :: Parser String
row = do
  optional newline
  time <- between '[' ']' (many anyChar)
  skipMany newline
  pure time

partOne = do
  print $ parseString (many row) mempty example
  print "sup"
