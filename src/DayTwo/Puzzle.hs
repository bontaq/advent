module DayTwo.Puzzle (partOne, partTwo) where

import Text.Trifecta.Parser (Parser, parseFromFile)
import Text.Parser.Combinators (many, eof, manyTill)
import Text.Parser.Char (anyChar, newline)
import Data.List
import Data.Char (ord)
import Data.Maybe (isJust)

words' :: Parser [Char]
words' = manyTill anyChar newline

score :: [[a]] -> (Int, Int)
score characters =
  let
    scoreA = length . filter ((== 2) . length) $ characters
    scoreB = length . filter ((== 3) . length) $ characters
    scoreA' = if scoreA > 0 then 1 else 0
    scoreB' = if scoreB > 0 then 1 else 0
  in
    (scoreA', scoreB')

sumPairs :: [(Int, Int)] -> (Int, Int)
sumPairs = foldr (\(a', b') (a, b) -> (a' + a, b' + b)) (0, 0)

multiplyPair :: (Int, Int) -> Int
multiplyPair (a, b) = a * b

partOne = do
  raw <- parseFromFile (many words')  "./src/DayTwo/Data.txt"
  print $ (fmap $ multiplyPair . sumPairs) $ (fmap . fmap) (score . group . sort) $ raw

differenceOfOne :: [Char] -> [Char] -> Bool
differenceOfOne a b = (== 1) . length $ (\\) a b

valid a b =
  let countDiffs = (\(a, b) acc -> if a == b then acc else acc + 1)
  in (== 1) $ foldr countDiffs 0 $ zip a b

validPair :: [Char] -> [Char] -> Maybe (String, String)
validPair a b =
  case valid a b of
    True  -> Just (a, b)
    False -> Nothing

partTwo = do
  raw <- parseFromFile (many words') "./src/DayTwo/Data.txt"
  -- bunch of Nothing[]
  let answers = fmap (\items -> map (\cursor -> map (validPair cursor) items) items) $ raw
  -- clean it up
  let answer = fmap (head . concat . map (filter (isJust))) answers
  -- get the intersection on the answer
  let finalAnswer = fmap (fmap (\(a, b) -> intersect a b)) answer
  -- clap clap
  print finalAnswer
