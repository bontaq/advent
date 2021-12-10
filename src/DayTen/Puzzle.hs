module DayTen.Puzzle where

import Data.List
import Data.Maybe
import Data.Bifunctor

parse = lines <$> readFile "./src/DayTen/data.txt"

--------------
-- Part One --
--------------

pairs = fmap (\(open:close:_) -> (open, close)) ["[]", "()", "{}", "<>"]

openers = fmap fst pairs

findOpener closer = fst . fromJust $ find (\(_, closer') -> closer == closer') pairs
findCloser opener = snd . fromJust $ find (\(opener', _) -> opener == opener') pairs

points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137

corrupt :: String -> String -> Maybe Char
corrupt [] _ = Nothing
corrupt (c:cs) stack =
  if c `elem` openers
  then corrupt cs (c : stack)
  else
    let
      opener = findOpener c
      (closing:rest) = stack
    in
      if closing == opener
      then corrupt cs rest
      else Just c

partOne = do
  lines <- parse

  print $ sum $ fmap points $ catMaybes $ fmap (\line -> corrupt line "") lines

points' '(' = 1
points' '[' = 2
points' '{' = 3
points' '<' = 4

finishLine :: String -> String -> String
finishLine [] stack = stack
finishLine (c:cs) stack =
  if c `elem` openers
  then finishLine cs (c : stack)
  else
    let
      opener = findOpener c
      (closing:rest) = stack
    in
      if closing == opener
      then finishLine cs rest
      else error "should not happen with unfinished"

score :: [Int] -> Int
score = foldl (\total num -> (total * 5) + num) 0

partTwo = do
  lines <- parse

  let unfinishedLines = fmap fst
        $ filter (isNothing . snd)
        $ fmap (\line -> (line, corrupt line "")) lines
      scores = fmap score
        $ fmap (\line -> fmap points' $ finishLine line "") unfinishedLines

  print $ (!! (div (length scores) 2)) $ sort scores
