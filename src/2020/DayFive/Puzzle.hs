module DayFive.Puzzle where

import Data.List hiding (partition)
import Data.Maybe

rows = [0..127]

partition :: [a] -> ([a], [a])
partition rows = splitAt (length rows `div` 2) rows

-- first 7 chars are F or B
findRow :: String -> [Integer] -> Integer
findRow []    [row] = row
findRow (c:cs) rows = case c of
  'F' -> findRow cs (fst $ partition rows)
  'B' -> findRow cs (snd $ partition rows)

columns = [0..7]

findColumn :: String -> [Integer] -> Integer
findColumn []    [column] = column
findColumn (c:cs) columns = case c of
  'L' -> findColumn cs (fst $ partition columns)
  'R' -> findColumn cs (snd $ partition columns)

findSeat directions =
  let (rowDirections, columnDirections) = splitAt 7 directions
      row = findRow rowDirections rows
      column = findColumn columnDirections columns
  in (row * 8) + column

partOne = do
  passes <- lines <$> readFile "./src/DayFive/data.txt"
  print $ maximum $ fmap findSeat passes

toSeatId (row, column) = (row * 8) + column

allSeats = [ toSeatId (a, b) | a <- rows
                             , b <- columns ]

-- just do the set difference :shrugcity:
findMySeat seats = allSeats \\ seats

partTwo = do
  passes <- lines <$> readFile "./src/DayFive/data.txt"
  -- I just looked at the result to see the one in the middle
  -- dunno what the programmatic way to do that is :think:
  print $ findMySeat $ fmap findSeat passes
