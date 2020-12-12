module DayTen.Puzzle where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

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

walk' :: Integer -> Map Integer Integer -> [Integer] -> Integer
walk' _ _ [] = 1
walk' 49 _ _ = 1
walk' voltage prev adapters =
  let
    one = find (== voltage + 1) adapters
    two = find (== voltage + 2) adapters
    three = find (== voltage + 3) adapters
  in
    sum
    $ fmap (\(Just x) ->
              case Map.lookup x prev of
                Just y -> y
                _ ->
                  let
                    val = walk' x prev (delete x adapters)
                  in 
                    Map.insert x val prev
                    pure val
           )
    $ filter isJust [one, two, three]


partTwo = do
  nums <- (fmap (\s -> read s :: Integer)) <$> lines <$> readFile "./src/DayTen/data.txt"
  print $ walk' 0 Map.empty nums
  pure ()
