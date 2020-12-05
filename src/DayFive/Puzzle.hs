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

findSeat' directions =
  let (rowDirections, columnDirections) = splitAt 7 directions
      row = findRow rowDirections rows
      column = findColumn columnDirections columns
  in (row, column)

toSeatId (row, column) = (row * 8) + column

allSeats = [ toSeatId (a, b) | a <- rows
                             , b <- columns
                             , a /= 0
                               -- very weird
                             , (a >= 122) == False ]

findMySeat seats = allSeats \\ seats

partTwo = do
  passes <- lines <$> readFile "./src/DayFive/data.txt"
  let seats = fmap findSeat' passes
      seatIds = fmap toSeatId seats
  print $ findMySeat seatIds
