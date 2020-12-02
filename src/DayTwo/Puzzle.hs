module DayTwo.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Char
import Text.Trifecta.Parser

import Data.Maybe


type MinOccur = Int
type MaxOccur = Int
type Character = Char
type Password = String

type Row = (MinOccur, MaxOccur, Character, Password)

parseRow :: Parser Row
parseRow = do
  minOccur  <- token integer
  _         <- char '-'
  maxOccur  <- token integer
  character <- letter
  _         <- string ": "
  password  <- many letter

  pure ( fromInteger minOccur
       , fromInteger maxOccur
       , character
       , password
       )

parseRows :: Parser [Row]
parseRows = many (token parseRow)

validCheck :: Row -> Bool
validCheck (minOccur, maxOccur, character, password) =
  let occurences = length $ filter (== character) password
  in occurences <= maxOccur && occurences >= minOccur

partOne :: IO ()
partOne = do
  rows <- fromJust <$> parseFromFile parseRows "./src/DayTwo/data.txt"
  print $ length $ filter validCheck rows

newPolicy :: Row -> Bool
newPolicy (firstPosition, secondPosition, character, password) =
  let
    firstChar = password !! (firstPosition - 1)
    secondChar = password !! (secondPosition - 1)
  in
    (length $ filter (== character) [firstChar, secondChar]) == 1

partTwo :: IO ()
partTwo = do
  rows <- fromJust <$> parseFromFile parseRows "./src/DayTwo/data.txt"
  print $ length $ filter newPolicy rows
