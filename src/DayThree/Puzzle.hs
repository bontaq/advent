module DayThree.Puzzle where

import GHC.Base (divInt)

-----------
-- Parse --
-----------

fileLocation = "./src/DayThree/testData.txt"

toDigits :: [Char] -> [Int]
toDigits = fmap (\char -> read [char])

parse = fmap toDigits . lines <$> readFile fileLocation


--------------
-- Part One --
--------------

combine a b = plus <$> zip a b
  where plus (a,b) = a + b

oneOrZero size value = if value < (size `divInt` 2) then 0 else 1

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
