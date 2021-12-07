module DaySeven.Puzzle where

import Data.List.Split

-----------
-- Parse --
-----------
fileLocation = "./src/DaySeven/data.txt"

parse :: IO [Int]
parse = fmap read . splitOn "," <$> readFile fileLocation

--------------
-- Part One --
--------------

moveCost :: Int -> [Int] -> Int
moveCost position = sum . fmap (\x -> abs $ position - x)

lowest crabs = minimum $ fmap (`moveCost` crabs) [0..(maximum crabs)]

partOne = lowest <$> parse

--------------
-- Part Two --
--------------

moveCost' :: Int -> [Int] -> Int
moveCost' position = sum . fmap (\x -> (distance x * (distance x + 1)) `div` 2)
  where distance x = abs (position - x)

lowest' crabs = minimum $ fmap (`moveCost'` crabs) [0..(maximum crabs)]

partTwo = lowest' <$> parse
