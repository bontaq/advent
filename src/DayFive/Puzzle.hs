module DayFive.Puzzle where

import Control.Monad

window :: Int -> [a] -> [[a]]
window n xs
  | length xs >= n = take n xs : window n (drop 1 xs)
  | otherwise = []

--
-- How many units remain after fully reacting the polymer you scanned?
--
partOne = print "sup"
