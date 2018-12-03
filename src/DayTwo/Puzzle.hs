module DayTwo.Puzzle (partOne, partTwo) where

import Text.Trifecta.Parser (Parser, parseFromFile)
import Text.Parser.Combinators (many, eof, manyTill)
import Text.Parser.Char (anyChar, newline)
import Data.List
import Data.Char (ord)

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

toDigits = map ord

validPair :: [Char] -> [Char] -> Bool
validPair a b = (== 1) . length $ (\\) a b

partTwo = do
  raw <- parseFromFile (many words') "./src/DayTwo/Data.txt"
  print $ fmap (map (map validPair)) raw
