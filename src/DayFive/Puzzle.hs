module DayFive.Puzzle where

import Control.Monad
import Data.Char
import Data.List
import Control.Applicative

window :: Int -> [a] -> [[a]]
window n xs
  | length xs >= n = take n xs : window n (drop 1 xs)
  | otherwise = []

collapse a b
  | isUpper a /= isUpper b = toUpper a == toUpper b
  | otherwise = False

pass :: [Char] -> [Char]
pass (a:b:rest) =
  case collapse a b of
    True -> pass rest
    False -> a : (pass (b : rest))
pass a = a

clean :: [Char] -> [Char]
clean raw =
  let cleaned = pass raw
  in if cleaned == raw
     then cleaned
     else clean cleaned

--
-- How many units remain after fully reacting the polymer you scanned?
--
partOne = do
  -- amusingly seems to insert a newline?
  input <- readFile "./src/DayFive/Data.txt"
  print $ length input
  print $ length . clean $ input

unique :: [Char] -> [Char]
unique = foldr (\a acc ->  if elem a acc then acc else a : acc) []

filterClean :: Char -> [Char] -> [Char]
filterClean c = clean . filter (\a -> (a /= c) && (a /= (toLower c)))

--
-- What is the length of the shortest polymer you can produce by
-- removing all units of exactly one type and fully reacting the result?
--
partTwo = do
  input <- readFile "./src/DayFive/Data.txt"
  let noNewline = filter (/= '\n') input
      -- surprise it's the alphabet
      kinds = sort . unique . map toUpper $ unique noNewline

      -- collapsed
      filters = map filterClean kinds
      filtered = map ($ noNewline) filters
      answers = zip kinds (map length filtered)

  print answers
