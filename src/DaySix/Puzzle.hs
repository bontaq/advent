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

test = [r|
Time:      7  15   30
Distance:  9  40  200
|]

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

partOne = do
  -- raw <- readFile location
  let
    result = parseString parseLines mempty test
    -- real = run' result

  -- print result
  -- print real
  print result
