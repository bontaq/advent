module DayTen.Puzzle where

import Control.Applicative
import Data.List
import Data.Maybe

-- cur voltage
walk :: Integer -> [Integer] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
walk _ [] count = count
walk voltage adapters (c1, c2, c3) =
  case (one, two, three) of
    (Just a, _, _) -> walk a (delete a adapters) (c1 + 1, c2, c3)
    (_, Just a, _) -> walk a (delete a adapters) (c1, c2 + 1, c3)
    (_, _, Just a) -> walk a (delete a adapters) (c1, c2, c3 + 1)
  where
    one = find (== voltage + 1) adapters
    two = find (== voltage + 2) adapters
    three = find (== voltage + 3) adapters

-- 2664 -- no
partOne = do
  nums <- (fmap (\s -> read s :: Integer)) <$> lines <$> readFile "./src/DayTen/data.txt"
  print $ walk 0 nums (0, 0, 0)
  pure ()

walk' :: Integer -> [Integer] -> Integer
walk' _ [] = 1
walk' 182 _ = 1
walk' voltage adapters =
  sum
  $ fmap (\(Just x) -> walk' x (delete x adapters))
  $ filter isJust [one, two, three]
  where
    one = find (== voltage + 1) adapters
    two = find (== voltage + 2) adapters
    three = find (== voltage + 3) adapters

partTwo = do
  nums <- (fmap (\s -> read s :: Integer)) <$> lines <$> readFile "./src/DayTen/data.txt"
  print $ maximum nums
  print $ walk' 0 nums
  pure ()
