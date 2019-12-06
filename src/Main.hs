module Main where

import qualified DayOne.Puzzle as DayOne
import qualified DayTwo.Puzzle as DayTwo
import qualified DayThree.Puzzle as DayThree

main :: IO ()
main = do
  -- DayTwo.partTwo
  res <- DayThree.partTwo
  print res
