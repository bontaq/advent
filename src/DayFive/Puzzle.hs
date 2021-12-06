module DayFive.Puzzle where

import Data.List

-----------
-- Parse --
-----------

fileLocation = "./src/DayFive/data.txt"

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn c cs = (takeWhile (/= c) cs, drop 1 (dropWhile (/= c) cs))

parseLine [start, _, end] =
  let
    readTuple (a, b) = (read a :: Int, read b :: Int)
  in
    readTuple <$> fmap (splitOn ',') [start, end]

parse = do
  rawLines <- fmap words . lines <$> readFile fileLocation
  pure $ fmap parseLine rawLines

--------------
-- Part One --
--------------

-- gen lines then intersect for "performance"
genPoints start end =
  if start < end then [start..end] else reverse [end..start]

genLine [(startX, startY), (endX, endY)] =
  -- fine for vertical or horizontal, diagonal gonna need to redo
  [(x, y) | x <- genPoints startX endX, y <- genPoints startY endY]

onlyStraightLines :: Eq a => [[(a, a)]] -> [[(a, a)]]
onlyStraightLines = filter (\[(a, b), (a', b')] -> a == a' || b == b')

intersections line lines =
  nub $ foldr (<>) [] $ fmap (intersect line) (filter (/= line) lines)

partOne = do
  coords <- parse

  let
    lines = fmap genLine (onlyStraightLines coords)
    intersectedPoints = fmap (\line -> intersections line lines) lines

  print (length $ nub $ foldr (<>) [] $ intersectedPoints)

--------------
-- Part Two --
--------------

diagonalLine' x y x' y' =
  let newX = if x < x' then x + 1 else x - 1
      newY = if y < y' then y + 1 else y - 1
  in (newX, newY) : if newX /= x' then diagonalLine' newX newY x' y' else []

diagonalLine x y x' y' = (x,y):diagonalLine' x y x' y'

genLine' [(startX, startY), (endX, endY)] =
  if startX == endX || startY == endY
  then [(x, y) | x <- genPoints startX endX, y <- genPoints startY endY]
  else diagonalLine startX startY endX endY

partTwo = do
  coords <- parse

  let
    lines = fmap genLine' coords
    intersectedPoints = fmap (\line -> intersections line lines) lines

  print (length $ nub $ foldr (<>) [] $ intersectedPoints)
  -- print (last lines)
