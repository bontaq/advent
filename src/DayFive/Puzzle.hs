{-# LANGUAGE QuasiQuotes #-}
-- |

module DayFive.Puzzle where

import Text.RawString.QQ

import Control.Applicative
import Text.Parser.Token
import Text.Trifecta

test = [r|
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|]

data Map = Map
  { seeds :: [Integer]
  , transforms :: [Transform]
  }
  deriving Show

type Transform = [[Integer]]

parseLine :: Parser [Integer]
parseLine = do
  nums <- sepBy integer' (char ' ')
  newline
  pure nums

parseTransform :: Parser Transform
parseTransform = do
  manyTill anyChar (char ':')
  newline
  rows <- some parseLine

  pure (filter (not . null) rows)

parseMaps :: Parser Map
parseMaps = do
  optional newline
  string "seeds: "
  seeds <- many integer
  transforms <- some parseTransform
  pure (Map seeds transforms)

partOne = do
  let
    result = parseString parseMaps mempty test

  print result
