module Puzzle where

import Text.Parser.Combinators (many)
import Text.Parser.Token (token, integer)
import Text.Trifecta.Parser (Parser, parseFromFile)


numbers :: Parser [Integer]
numbers = many (token integer)

partOne = do
  nums <- parseFromFile numbers "./src/DayOne/data.txt"
  pure nums
