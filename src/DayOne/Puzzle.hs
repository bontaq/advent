module DayOne.Puzzle (partOne, partTwo) where

import qualified Data.Map.Strict as Map
import           Text.Parser.Combinators (many)
import           Text.Parser.Token (token, integer)
import           Text.Trifecta.Parser (Parser, parseFromFile)

numbers :: Parser [Integer]
numbers = many (token integer)

partOne = do
  readNumbers <- parseFromFile numbers "./src/DayOne/PartOneData.txt"
  print $ readNumbers >>= pure . sum

type Seen = Map.Map Integer Bool
type Answer = Integer
type Frequency = Integer

findFrequency' :: Seen -> Frequency -> [Integer] -> Maybe Answer
findFrequency' _ _ []           = Nothing
findFrequency' seen freq (x:xs) =
  let newFreq = x + freq
  in case Map.lookup newFreq seen of
    Just _  -> Just newFreq
    Nothing -> findFrequency' (Map.insert newFreq True seen) newFreq xs

findFrequency :: [Integer] -> Maybe Answer
findFrequency = findFrequency' (Map.empty) 0

partTwo :: IO ()
partTwo = do
  readNumbers <- parseFromFile numbers "./src/DayOne/PartOneData.txt"
  print $ fmap (findFrequency . cycle) readNumbers
