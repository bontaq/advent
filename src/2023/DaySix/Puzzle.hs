{-# LANGUAGE QuasiQuotes #-}
-- |

module DaySix.Puzzle where

import Text.RawString.QQ

import Control.Applicative
import Text.Parser.Token
import Text.Trifecta

import Debug.Trace

input = [r|
Time:        49     78     79     80
Distance:   298   1185   1066   1181
|]

input2 = [r|
Time:       49787980
Distance:   298118510661181
|]

test = [r|
Time:      7  15   30
Distance:  9  40  200
|]

test2 = [r|
Time:      71530
Distance:  940200
|]

scores time =
  let
    holds = [1..time]
  in
    fmap (\held -> held * (time - held)) holds

parseLines :: Parser [(Integer, Integer)]
parseLines = do
  newline
  string "Time:"
  whiteSpace
  times <- sepBy integer whiteSpace

  string "Distance:"
  whiteSpace
  distances <- sepBy integer whiteSpace

  pure $ zip times distances

runRace (time, distance) =
  length $ filter ((<) distance) (scores time)



partOne = do
  -- raw <- readFile location
  let
    result = parseString parseLines mempty input2
    wins = fmap runRace <$> result

  -- print result
  -- print real
  print result
  print wins
  print (foldr (*) 1 <$> wins)
