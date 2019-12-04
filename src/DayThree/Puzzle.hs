module DayThree.Puzzle where

import Prelude hiding (Right, Left)
import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char, oneOf)
import Text.Trifecta.Parser (Parser, parseFromFile, parseString)
import Text.Trifecta.Result
import Data.List (intersect)
import qualified Data.Set as S

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

rev :: Integer -> [Integer] -> [Integer]
rev 0 bs = 0 : bs
rev a bs = (a) : rev (a - 1) bs

walk :: [(Direction, Distance)] -> Location -> S.Set Location -> S.Set Location
walk ((direction, distance):remainingDirections) lastLoc locs =
  let
    (lastX, lastY) = lastLoc
  in
    case direction of
      Right ->
        let
          newLocs = [ (lastX + x, lastY) | x <- rev distance [] ]
        in
          walk remainingDirections (head newLocs) (S.fromList newLocs) <> locs
      Left  ->
        let newLocs = [ (lastX - x, lastY) | x <- rev distance [] ]
        in
          walk remainingDirections (head newLocs) (S.fromList newLocs) <> locs
      Up    ->
        let newLocs = [ (lastX, lastY + y) | y <- rev distance [] ]
        in
          walk remainingDirections (head newLocs) (S.fromList newLocs) <> locs
      Down  ->
        let newLocs = [ (lastX, lastY - y) | y <- rev distance [] ]
        in
          walk remainingDirections (head newLocs) (S.fromList newLocs) <> locs
walk _ _ locs        = locs

calcDistance (0, 0) = 99999
calcDistance (a, b) = abs(a) + abs(b)

partOne :: IO ()
partOne = do
   f <- readFile "./src/DayThree/data.txt"
   let parsed = fmap (parseString pairs mempty) (lines f)
       (w:ws) = fmap (\x -> fmap (\ds -> walk ds (0, 0) mempty) x) parsed
       intersected = foldr (\(Success a) (Success b) -> Success (S.intersection a b)) w ws

   case intersected of
     Success a -> do
       let distances = fmap calcDistance (S.toList a)
       print $ minimum distances

   pure ()
