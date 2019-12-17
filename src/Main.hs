module Main where

import qualified DayOne.Puzzle as DayOne
import qualified DayTwo.Puzzle as DayTwo
import qualified DayThree.Puzzle as DayThree
import qualified DaySeven.Puzzle as DaySeven

main :: IO ()
main = do
  -- DayTwo.partTwo
  res <- DaySeven.partTwo
  print res
