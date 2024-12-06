{-# LANGUAGE QuasiQuotes #-}

module DayThree.Puzzle where

import Text.RawString.QQ
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Trifecta.Parser
import Control.Applicative

location = "./src/DayThree/data.txt"

test = [r|
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)
+mul(32,64]then(mul(11,8)mul(8,5))
|]

test2 = [r|
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
|]

data Results = None | Enable | Disable | Ans Integer
  deriving Show

combined :: Parser Results
combined = do
  string "mul("
  firstNumber <- integer
  char ','
  secondNumber <- integer
  char ')'
  pure (Ans $ firstNumber * secondNumber)

pointless :: Parser Results
pointless = do
  anyChar
  pure None

disable :: Parser Results
disable = do
  string "don't()"
  pure Disable

enable :: Parser Results
enable = do
  string "do()"
  pure Enable

parser =
  try combined <|> try enable <|> try disable <|> pointless

sumResults [] = 0
sumResults (Ans num:rest) = num + sumResults rest
sumResults (_:rest) = sumResults rest

partOneEx = do
  let results = parseString (many parser) mempty test
  print $ sumResults <$> results
  pure ()

partOne = do
  raw <- readFile location
  let results = parseString (many parser) mempty raw
  print $ sumResults <$> results
  pure ()

sumResults2 enabled [] = 0
sumResults2 enabled (Ans num:rest) = (if enabled then num else 0) + sumResults2 enabled rest
sumResults2 enabled (Enable:rest)  = sumResults2 True rest
sumResults2 enabled (Disable:rest) = sumResults2 False rest
sumResults2 enabled (_:rest)       = sumResults2 enabled rest

partTwoEx = do
  let results = parseString (many parser) mempty test2
  print $ sumResults2 True <$> results
  pure ()

partTwo = do
  raw <- readFile location
  let results = parseString (many parser) mempty raw
  print $ sumResults2 True <$> results
  pure ()
