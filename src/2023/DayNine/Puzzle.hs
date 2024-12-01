{-# LANGUAGE QuasiQuotes #-}
-- |

module DayNine.Puzzle where

import Text.RawString.QQ

import Control.Applicative
import Text.Parser.Token
import Text.Trifecta

import Debug.Trace
import Data.List
import Data.Maybe
import Data.List.Extra
import Data.List.Split

test = [r|
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
|]

test2 = [r|
19 39 67 107 181 348 723 1500 3006 5848 11264 21858 42998 85299 168818 329877 631829 1181621 2154721 3831901 6652547
|]

parseInput :: String -> [[Integer]]
parseInput str =
  let rows = words <$> lines str
  in fmap read <$> rows

diffs [] = []
diffs [_] = []
diffs (a:b:rest) = [b - a] <> diffs (b:rest)

expand :: [Integer] -> [[Integer]]
expand nums =
  let
    nextRow = diffs nums
  in
    if all (== 0) nextRow
    then [nums]
    else [nums] <> expand nextRow

nextNum :: Integer -> [[Integer]] -> Integer
nextNum addition [] = addition
nextNum addition (row:rows) =
  let newNum = last row + addition
  in nextNum newNum rows

runIt item =nextNum 0 $ reverse $ expand item

location = "./src/DayNine/data.txt"

partOne = do
  raw <- readFile location

  let
    puzzle = filter (not . null) $ parseInput raw
    newNums = fmap runIt puzzle

  print newNums
  print $ sum newNums

prevNum :: Integer -> [[Integer]] -> Integer
-- prevNum addition rows | trace (show addition <> " " <> show rows) False = undefined
prevNum addition [] = addition
prevNum addition (row:rows) =
  let newNum = head row - addition
  in prevNum newNum rows

runIt' item = prevNum 0 $ reverse $ expand item

partTwo = do
  raw <- readFile location

  let
    puzzle = filter (not . null) $ parseInput raw
    newNums = fmap runIt' puzzle

  print newNums
  print $ sum newNums
