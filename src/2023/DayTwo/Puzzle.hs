{-# LANGUAGE QuasiQuotes #-}
-- |

module DayTwo.Puzzle where

import Text.RawString.QQ (r)
import Text.Trifecta.Parser (Parser, parseString, parseFromFile)
import Control.Applicative
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Trifecta.Result (Result)

test = [r|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|]

type Count = Integer

data Color = Blue | Red | Green
  deriving (Show, Eq)

type Cube = (Count, Color)

data Row = Row
  { gameId :: Integer
  , reveal :: [[Cube]]
  }
  deriving Show

blue  = do
  string "blue"
  pure Blue
red   = do
  string "red"
  pure Red
green = do
  string "green"
  pure Green

parseCubes :: Parser Cube
parseCubes = do
  count <- integer

  color <- blue <|> red <|> green

  optional (string ", ")

  pure (count, color)

parseReveal :: Parser [Cube]
parseReveal = do
  cube <- many parseCubes
  optional $ string "; " <|> string ";"
  pure cube

parseReveals :: Parser [[Cube]]
parseReveals = manyTill parseReveal newline

parseRow :: Parser Row
parseRow = do
  optional newline

  string "Game "
  gameId <- integer
  string ": "
  reveals <- parseReveals

  pure $ Row gameId reveals

cubeIsPossible (count, color) =
  case color of
    Red   -> count <= 12
    Green -> count <= 13
    Blue  -> count <= 14

revealIsPossible :: [Cube] -> Bool
revealIsPossible = all cubeIsPossible

gameIsPossible :: [[Cube]] -> Bool
gameIsPossible games =
  let revealsPossible = fmap revealIsPossible games
  in and revealsPossible

location = "./src/2023/DayTwo/data.txt"

partOne :: IO String
partOne = do
  input <- readFile location

  let
    result = parseString (many parseRow) mempty input :: Result [Row]
    results = filter (gameIsPossible . reveal) <$> result
    gameIds = fmap gameId <$> results

  pure $ show (sum <$> gameIds)

minimumCount :: Color -> [[Cube]] -> Integer
minimumCount color game =
  let
    matches = fst <$> concatMap (filter (\(count, color') -> color' == color)) game
  in
    maximum matches

minsForGame :: [[Cube]] -> Integer
minsForGame game =
  let
    reds = minimumCount Red game
    blues = minimumCount Blue game
    greens = minimumCount Green game
  in
    reds * blues * greens

partTwo :: IO String
partTwo = do
  input <- readFile location

  let
    result = parseString (many parseRow) mempty input :: Result [Row]
    results = fmap (minsForGame . reveal) <$> result

  pure $ show (sum <$> results)
