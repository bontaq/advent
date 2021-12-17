{-# LANGUAGE QuasiQuotes #-}
module DayTwelve.Puzzle where

import Text.RawString.QQ (r)
import Data.List.Split
import Data.Char
import Data.List

smallSample = [r|
  start-A
  start-b
  A-c
  A-b
  b-d
  A-end
  b-end
|]

slightlyBigSample = [r|
  dc-end
  HN-start
  start-kj
  dc-start
  dc-HN
  LN-dc
  HN-end
  kj-sa
  kj-HN
  kj-dc
|]

biggerSample = [r|
  fs-end
  he-DX
  fs-he
  start-DX
  pj-DX
  end-zg
  zg-sl
  zg-pj
  pj-he
  RW-he
  fs-DX
  pj-RW
  zg-RW
  start-pj
  he-WI
  zg-he
  pj-fs
  start-RW
|]

parse :: String -> [[String]]
parse =  filter ((==) 2 . length) <$> fmap (splitOn "-" . dropWhile isSpace) <$> lines

isStart [a, b] = a == "start" || b == "start"
isEnd [a, b] = a == "end" || b == "end"

isBig :: String -> Bool
isBig = all isUpper

isSmall :: String -> Bool
isSmall = all isLower

order start [a, b]
  | start == b = [b, a]
  | otherwise  = [a, b]

nextSegments :: String -> [[String]] -> [String]
nextSegments start map =
  (!! 1) . order start <$> filter (\[a, b] -> a == start || b == start) map

removeSmalls visited = filter (\p -> isBig p || p `notElem` visited)

loop :: String -> [String] -> [[String]] -> [String]
loop "end" visited _   = visited
loop start visited map =
  let nextStarts = removeSmalls visited $ nextSegments start map
  in concat $ fmap (\s -> loop s (start:visited) map) nextStarts

partOne = do
  f <- readFile "./src/DayTwelve/data.txt"
  pure $ loop "start" [] (parse f)

removeSmalls' visited =
  filter (\p -> (isBig p || not visitedTwice || p `notElem` visited) && p /= "start")
  where visitedTwice = any ((>= 2) . length) . group . sort . filter isSmall $ visited

loop' :: String -> [String] -> [[String]] -> [[String]]
loop' "end" visited _   = [visited]
loop' start visited map =
  let nextStarts = removeSmalls' (start:visited) $ nextSegments start map
  in concat $ fmap (\s -> loop' s (start:visited) map) nextStarts

partTwo = do
  f <- readFile "./src/DayTwelve/data.txt"
  print $ length $ loop' "start" [] (parse f)
