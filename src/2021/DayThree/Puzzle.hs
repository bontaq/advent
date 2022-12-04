module DayThree.Puzzle where

import GHC.Base (divInt)
import Debug.Trace

-----------
-- Parse --
-----------

fileLocation = "./src/DayThree/data.txt"

toDigits :: [Char] -> [Int]
toDigits = fmap (\char -> read [char])

parse = fmap toDigits . lines <$> readFile fileLocation


--------------
-- Part One --
--------------

combine a b = plus <$> zip a b
  where plus (a,b) = a + b

convert :: Int -> Double
convert = fromIntegral

oneOrZero size value = if convert value < (convert size / 2) then 0 else 1

zeroOrOne size value = if value <= (size `divInt` 2) then 0 else 1

gamma (x:xs) =
  let totals = foldr combine x xs
      size = length (x:xs)
  in fmap (oneOrZero size) totals

epsilon = fmap opposite <$> gamma
  where
    opposite 1 = 0
    opposite 0 = 1

binaryToDecimal = sum . fmap combine . zip genPowers . reverse
  where
    genPowers = [ 2 ^ x | x <- [0..] ]
    combine (power, value) = power * value

partOne = do
  numbers <- parse
  print $ binaryToDecimal <$> [gamma numbers, epsilon numbers]


--------------
-- Part Two --
--------------

oxygenRating _       [x] = x
oxygenRating position xs =
  let mostCommon = gamma xs !! position
      remaining = filter (\x -> (x !! position) == mostCommon) xs
  in oxygenRating (position + 1) remaining

co2Rating _       [x] = x
co2Rating position xs =
  let mostCommon = epsilon xs !! position
      remaining = filter (\x -> (x !! position) == mostCommon) xs
  in co2Rating (position + 1) remaining

partTwo = do
  numbers <- parse
  print $ binaryToDecimal <$> [oxygenRating 0 numbers, co2Rating 0 numbers]
