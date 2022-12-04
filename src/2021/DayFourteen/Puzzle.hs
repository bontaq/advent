module DayFourteen.Puzzle where

import Data.List.Split
import Data.List hiding (insert, foldl)
import Data.Map hiding (drop, take, foldl)

fileLocation = "./src/DayFourteen/testData.txt"

parseRule str =
  let [match, _, new] = words str
  in (match, new)

parse = do
  [[polymer], rules] <- splitOn [""] . lines <$> readFile fileLocation

  pure (polymer, fmap parseRule rules)

pairs [_] = []
pairs xs = take 2 xs : pairs (drop 1 xs)

findRule pair = find (\(pair', _) -> pair == pair')

step [x] _ = [x]
step (a:b:rest) rules =
  let toInsert = findRule [a, b] rules
  in case toInsert of
    Just (_, newChar) -> a : newChar <> step (b:rest) rules
    Nothing           -> a : step (b:rest) rules

partOne = do
  (polymer, rules) <- parse

  let applied = iterate (`step` rules) polymer !! 10
      lengths = sort . fmap length . group . sort $ applied
      max = last lengths
      min = head lengths

  print $ max - min

-- so of course that doesn't work for part 2

-- we can't keep the full string in memory, instead we need counts
-- step' [x] _ = [x]

addKey :: String -> Map String Int -> Map String Int
addKey key map =
  if key `member` map
  then adjust (\x -> x + x) key map
  else insert key 1 map

step' :: String -> [(String, String)] -> Map String Int -> Map String Int
step' (a:b:_) rules map =
  let toInsert = findRule [a, b] rules
  in case toInsert of
    Just (_, newChar) -> addKey (b : newChar) map
    Nothing           -> addKey [a, b] map

steps' :: [(String, String)] -> Map String Int -> Map String Int
steps' rules map = foldl (\map' key -> step' key rules map') map (keys map)

partTwo = do
  (polymer, rules) <- parse

  let
    map = fromList $ zip (pairs polymer) (repeat 1)
    applied = iterate (steps' rules) map !! 10

  print applied
