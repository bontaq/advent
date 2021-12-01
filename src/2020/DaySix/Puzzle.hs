module DaySix.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Char
import Text.Trifecta.Parser

import Data.List
import Control.Applicative

eop :: Parser String
eop = (count 2 newline) <|> (eof >> return [])

someTill p end = liftA2 (:) p (manyTill p end)

parseGroups :: Parser String
parseGroups = filter (/= '\n') <$> someTill anyChar (try eop)

partOne = do
  parsed <- parseFromFile (many parseGroups) "./src/DaySix/data.txt"
  print $ fmap (sum . fmap (length . nub)) parsed

parseGroups' :: Parser [String]
parseGroups' = lines <$> someTill anyChar (try eop)

allAnswered :: [String] -> Int
allAnswered = length . foldr1 intersect

partTwo = do
  parsed <- parseFromFile (many parseGroups') "./src/DaySix/data.txt"
  print $ fmap (sum . fmap allAnswered) parsed
