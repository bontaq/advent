module DayEight.Puzzle where

import Data.List (sortBy)

-- 25 pixels wide and 6 pixels tall.

type Width = Int
type Height = Int

infini f x = f x : infini f (f x)

group :: Int -> [a] -> [[a]]
group _ [] = []
group x p =
  let
    (row, rest) = splitAt x p
  in
    row : group x rest

layerize :: Width -> Height -> [a] -> [[[a]]]
layerize x y p =
  let
    rows = group x p
    layers = group y rows
  in
    layers

testInput = "123456789012"

parse :: String -> [Int]
parse = fmap (\x -> read (x : "") :: Int)

-- the Elves would like you to find the layer that contains the fewest 0 digits. On that layer, what is the number of 1 digits multiplied by the number of 2 digits?

testLayerize = layerize 3 2 (parse testInput)

filterReturns = filter (/= '\n')

countZeroes = length . filter (== 0)

compareZeroes a b = compare (countZeroes a) (countZeroes b)

countOnes = length . filter (== 1)
countTwos = length . filter (== 2)

calc a =
  let
    ones = countOnes a
    twos = countTwos a
  in
    ones * twos

partOne :: IO ()
partOne = do
  f <- readFile "./src/DayEight/data.txt"
  let parsed = parse $ filterReturns f
      layerized = layerize 25 6 parsed
      combined = fmap concat layerized
      sorted = sortBy compareZeroes combined
      calced = fmap calc sorted
  print $ calced

  pure ()

group' :: [[a]] -> [[a]]
group' arr =
  let
    range = [0..length arr - 1]
    width = [0..(length $ head arr) - 1]
    values = fmap (\y -> fmap (\x -> (arr !! x) !! y) range) width
  in
    values

determineColor :: [Int] -> Int
determineColor xs = foldr color 2 (reverse xs)
  where
    color a 2 = a
    color _ 0 = 0
    color _ 1 = 1

partTwo :: IO ()
partTwo = do
  f <- readFile "./src/DayEight/data.txt"
  let parsed = parse $ filterReturns f
      layerized = layerize 25 6 parsed
      combined = fmap concat layerized
      grouped = group' combined
      colorized = fmap determineColor grouped
      grouped' = head $ layerize 25 6 colorized

  mapM (\x -> print x) grouped'

  pure ()
