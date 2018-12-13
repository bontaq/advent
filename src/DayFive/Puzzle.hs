module DayFive.Puzzle where

import Control.Monad
import Data.Char

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
  print $ clean $ input
