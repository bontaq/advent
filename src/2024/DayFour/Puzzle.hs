{-# LANGUAGE QuasiQuotes #-}

module DayFour.Puzzle where

import Text.RawString.QQ
import Debug.Trace
import Data.List
import Control.Monad

location = "./src/DayFour/data.txt"

test = [r|
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
|]

test2 = [r|
xxyxx
xyxyx
yxxxy
|]

-- rotation would be
-- padding over by one, each row
-- then we can flip and pad for the other way

diagonal' :: Int -> [String] -> [String]
diagonal' _ [] = []
diagonal' level (line:rest) =
  let
    offset = take level (repeat ' ')
    newLevel = offset <> line
  in
    newLevel : diagonal' (level + 1) rest

diagonal :: [String] -> [String]
diagonal = diagonal' 0

mirrorVertical :: [String] -> [String]
mirrorVertical = fmap reverse

mirrorHorizontal :: [String] -> [String]
mirrorHorizontal = reverse

pretty str = do
  mapM_ print str
  pure ()

findXmas :: String -> Int
findXmas ('X':'M':'A':'S':rest) = 1 + findXmas rest
findXmas (c:rest)               = 0 + findXmas rest
findXmas []                     = 0

checks :: [String] -> [Int]
checks strs =
  let
    horizontals = sum $ fmap findXmas strs
    backwards = sum $ fmap findXmas $ mirrorVertical strs
    diagonals = sum
      $ fmap findXmas
      $ transpose
      $ diagonal strs
    diagonals' = sum
      $ fmap findXmas
      $ transpose
      $ diagonal
      $ mirrorHorizontal strs
    diagonals'' = sum
      $ fmap findXmas
      $ transpose
      $ diagonal
      $ mirrorVertical strs
    diagonals''' = sum
      $ fmap findXmas
      $ transpose
      $ diagonal
      $ mirrorHorizontal
      $ mirrorVertical strs
    verticals = sum
      $ fmap findXmas
      $ transpose strs
    verticals' = sum
      $ fmap findXmas
      $ transpose
      $ mirrorHorizontal strs
  in
    [horizontals , backwards
    , diagonals , diagonals'
    , diagonals'' , diagonals'''
    , verticals, verticals' ]

partOneEx = do
  print $ sum $ checks (lines test)
  pure ()

-- 2684 is too high
-- 2088 is too low

partOne = do
  raw <- lines <$> readFile location
  print $ sum $ checks raw
  pure ()

getBlock :: Int -> Int -> [String] -> [String]
getBlock x y strs =
  let
    verticals = take 3 (drop y strs)
    horizontals = fmap (take 3 . drop x) verticals
  in
    horizontals



temp2 = [r|
M.S
.A.
M.S

S.S
.A.
M.M

M.M
.A.
S.S

S.M
.A.
S.M
|]

check [['M', _, 'S'], [_, 'A', _], ['M', _, 'S']] = True
check [['S', _, 'S'], [_, 'A', _], ['M', _, 'M']] = True
check [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = True
check [['S', _, 'M'], [_, 'A', _], ['S', _, 'M']] = True
check _                                           = False

checkBlock :: [String] -> [[Bool]]
checkBlock raw =
  let
    ys = [0..length raw]
    xs = [0..length (raw !! 1)]
  in
    fmap (\y -> fmap (\x -> check $ getBlock x y raw) xs) ys

partTwoEx = do
  print $ length $ filter (== True) $ concat $ checkBlock (lines test)

partTwo = do
  raw <- readFile location
  print $ length $ filter (== True) $ concat $ checkBlock (lines raw)
