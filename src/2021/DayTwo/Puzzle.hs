{-# LANGUAGE NamedFieldPuns #-}
module DayTwo.Puzzle where

import Debug.Trace

-----------
-- Model --
-----------
type Distance = Integer

data Command
  = Up Distance
  | Down Distance
  | Forward Distance
  deriving Show

data Location = Location
  { horizontal :: Distance
  , depth      :: Distance
  , aim        :: Integer
  } deriving Show


-------------
-- Parsing --
-------------
inputFile = "./src/2021/DayTwo/data.txt"

parseLine [direction, distance] =
  let
    parsedDistance = read distance :: Integer
    parsedDirection = case direction of
      "up"      -> Up
      "down"    -> Down
      "forward" -> Forward
  in
    parsedDirection parsedDistance

parseFile =
  fmap parseLine <$> fmap words <$> lines <$> readFile inputFile


--------------
-- Part One --
--------------
handleCommand :: Location -> Command -> Location
handleCommand location@Location{ depth, horizontal } command =
  case command of
    Up y      -> location { depth=depth - y }
    Down y    -> location { depth=depth + y }
    Forward x -> location { horizontal=horizontal + x }

partOne = do
  commands <- parseFile

  let startLocation = Location { horizontal=0, depth=0, aim=0 }
      finalLocation = foldl handleCommand startLocation commands

  print finalLocation
  print $ (horizontal finalLocation) * (depth finalLocation)

--------------
-- Part Two --
--------------
handleCommand' :: Location -> Command -> Location
handleCommand' location@Location{ depth, horizontal, aim } command =
  case command of
    Up y      -> location { aim=aim - y }
    Down y    -> location { aim=aim + y }
    Forward x ->
      location { horizontal=horizontal + x
               , depth=depth + (aim * x)
               }

partTwo = do
  commands <- parseFile

  let startLocation = Location { horizontal=0, depth=0, aim=0 }
      finalLocation = foldl handleCommand' startLocation commands

  print finalLocation
  print $ (horizontal finalLocation) * (depth finalLocation)
