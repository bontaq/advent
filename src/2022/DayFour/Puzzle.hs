module DayFour.Puzzle where

import Data.List
import Data.List.Split

parsePart :: String -> [Int]
parsePart input =
  let [start, end] = read <$> splitOn "-" input
  in [start..end]

parseLine line =
  let parts = splitOn "," line
  in fmap parsePart parts

input = fmap parseLine . lines <$> readFile "./src/DayFour/data.txt"

fullyContained [a, b] = null (a \\ b) || null (b \\ a)

partOne = do
  ranges <- input

  let
    contained = fmap fullyContained ranges
    containedCount = length $ filter (== True) contained

  print containedCount

partiallyContained [a, b] = (> 0) $ length $ a `intersect` b

partTwo = do
  ranges <- input

  let
    contained = fmap partiallyContained ranges
    containedCount = length $ filter (== True) contained

  print containedCount
