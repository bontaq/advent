module DayFour.Puzzle where

import Data.List
import Debug.Trace

-- You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.
--
-- However, they do remember a few key facts about the password:
--
--     It is a six-digit number.
--     The value is within the range given in your puzzle input.
--     Two adjacent digits are the same (like 22 in 122345).
--     Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
--
-- Other than the range rule, the following are true:
--
--     111111 meets these criteria (double 11, never decreases).
--     223450 does not meet these criteria (decreasing pair of digits 50).
--     123789 does not meet these criteria (no double).
--
-- How many different passwords within the range given in your puzzle input meet these criteria?
--
-- Your puzzle input is 240920-789857.

increaseRule :: String -> Bool
increaseRule (a:b:cs) = (a <= b) && increaseRule (b:cs)
increaseRule _ = True

dupRule :: String -> Bool
dupRule s = (> 0) . length $ filter (== 2) $ fmap length $ group s

partOne :: IO ()
partOne = do
  let input = fmap show [240920..789857]
      ans = length $ filter (\x -> dupRule x && increaseRule x) input

  print ans
