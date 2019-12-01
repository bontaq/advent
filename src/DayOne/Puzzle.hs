module Puzzle where

import Text.Parser.Combinators (many)
import Text.Parser.Token (token, integer)
import Text.Trifecta.Parser (Parser, parseFromFile)
import Data.Maybe (fromJust)

numbers :: Parser [Integer]
numbers = many (token integer)

totalFuel :: Integer -> Integer
totalFuel f
  | f < 0 = 0
  | otherwise =
      let newFuel = ((div f 3) - 2)
      in f + (totalFuel newFuel)

partOne :: IO ()
partOne = do
  readNumbers <- parseFromFile numbers "./src/DayOne/data.txt"

  let ans = fmap (\x -> (div x 3) - 2) (fromJust readNumbers)
  print $ sum ans


partTwo :: IO ()
partTwo = do
  readNumbers <- parseFromFile numbers "./src/DayOne/data.txt"

  let ans = sum $ fmap (\x -> totalFuel $ (div x 3) - 2) (fromJust readNumbers)
  print ans
