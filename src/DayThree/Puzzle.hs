module DayThree.Puzzle where

import Prelude hiding (Right, Left)
import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char, oneOf)
import Text.Trifecta.Parser (Parser, parseFromFile, parseString)

data Direction = Right | Left | Up | Down
                 deriving Show
type Distance = Integer
type Location = (Integer, Integer)

parseDirectionChar :: Char -> Direction
parseDirectionChar 'R' = Right
parseDirectionChar 'L' = Left
parseDirectionChar 'U' = Up
parseDirectionChar 'D' = Down

pair :: Parser (Direction, Distance)
pair = do
  directionChar <- oneOf "RLDU"
  distance      <- integer
  _             <- optional (char ',')
  pure (parseDirectionChar directionChar, distance)

pairs :: Parser [(Direction, Distance)]
pairs = many pair

walk :: [(Direction, Distance)] -> [Location] -> [Location]
walk ((direction, distance):remainingDirections) locs@((lastX, lastY):_) =
  case direction of
    Right -> walk remainingDirections $ reverse [ (lastX + x, lastY) | x <- [0..distance] ] <> locs
    Left  -> walk remainingDirections $ reverse [ (lastX - x, lastY) | x <- [0..distance] ] <> locs
    Up    -> undefined
    Down  -> undefined
walk directions [] = walk directions [(0, 0)]
walk _ locs = locs

partOne = do
   f <- readFile "./src/DayThree/data.txt"
   let parsed = fmap (parseString pairs mempty) (lines f)
   print parsed

   pure ()
