{-# LANGUAGE QuasiQuotes #-}

module DayEight.Puzzle where

import Text.RawString.QQ

import Control.Applicative
import Text.Parser.Token
import Text.Trifecta

import Debug.Trace
import Data.List
import Data.Maybe
import Data.List.Extra
import Data.List.Split (endsWith)

import Data.Map.Strict (fromList, Map, (!))

input = [r|
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|]

input2 = [r|
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|]

input3 = [r|
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
|]

type Node = (String, String, String)

parseNode :: Parser Node
parseNode = do
  location <- some alphaNum

  string " = ("
  left <- some alphaNum
  string ", "
  right <- some alphaNum
  char ')'
  newline

  pure (location, left, right)


parseInput :: Parser (String, [Node])
parseInput = do
  optional newline
  instructions <- some alphaNum
  newline
  newline
  nodes <- many parseNode
  pure (instructions, nodes)

walk :: String -> Node -> [Node] -> [String]
walk _ ("ZZZ", _, _) _ = ["ZZZ"]
walk (direction:directions) (location, left, right) map =
  let
    nextStep = if direction == 'L' then left else right
    nextNode = find (\(location, _, _) -> location == nextStep) map
  in case nextNode of
    Just nextNode' -> [location] <> walk directions nextNode' map
    Nothing        -> error "could not find next location"

run (Success (instructions, map)) =
  let
    start = find (\(location, _, _) -> location == "AAA") map
  in case start of
    Just start' -> walk (cycle instructions) start' map
    Nothing     -> error "could not find start"

location = "./src/DayEight/data.txt"

partOne = do
  raw <- readFile location

  let
    puzzle = parseString parseInput mempty raw
    walked = run puzzle

  -- print puzzle
  -- print raw
  -- print walked
  print (length walked - 1)

step :: Char -> [Node] -> Node -> Node
step direction map (location, left, right) =
  let
    nextStep = if direction == 'L' then left else right
  in
    fromJust (find (\(location', _, _) -> nextStep == location') map)

done :: [([Char], b, c)] -> Bool
done = all (\(location, _, _) -> last location == 'Z')

steps :: Integer -> String -> [Node] -> [Node] -> Integer
-- steps _ (direction:_) places _ | trace (show direction  <> " " <> show places) False = undefined
steps count (direction:directions) places map =
  let
    newSteps = fmap (step direction map) places
  in
    if done newSteps
    then count + 1
    else steps (count + 1) directions newSteps map

run' (Success (instructions, map)) =
  let
    starts = filter (\(location, _, _) -> last location == 'A') map
    rans = fmap (\node -> steps 0 (cycle instructions) [node] map) starts
  in
    foldr lcm 1 rans

convert :: [Node] -> Map String Node
convert nodes = fromList $ fmap (\(loc, left, right) -> (loc, (loc, left, right))) nodes

partTwo = do
  raw <- readFile location

  let
    puzzle = parseString parseInput mempty raw
    walked = run' puzzle

  -- print puzzle
  -- print raw
  print walked
  -- print (walked + 1)
