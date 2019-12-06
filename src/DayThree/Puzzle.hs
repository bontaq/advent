module DayThree.Puzzle where

import Prelude hiding (Right, Left)
import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char, oneOf)
import Text.Trifecta.Parser (Parser, parseFromFile, parseString)
import Text.Trifecta.Result
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Debug.Trace

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
rev 0 bs = bs
rev a bs = a : rev (a - 1) bs

walk :: [(Direction, Distance)] -> Location -> S.Set Location -> S.Set Location
walk ((direction, distance):remainingDirections) lastLoc locs =
  let
    (lastX, lastY) = lastLoc
    newLocsRange = rev distance []
    newLocs = case direction of
      Right -> [ (lastX + x, lastY) | x <- newLocsRange ]
      Left  -> [ (lastX - x, lastY) | x <- newLocsRange ]
      Up    -> [ (lastX, lastY + y) | y <- newLocsRange ]
      Down  -> [ (lastX, lastY - y) | y <- newLocsRange ]
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

walkSeq :: [(Direction, Distance)] -> Location -> Q.Seq Location -> Q.Seq Location
walkSeq ((direction, distance):remainingDirections) lastLoc locs =
  let
    (lastX, lastY) = lastLoc
    newLocsRange = rev distance []
    newLocs = case direction of
      Right -> [ (lastX + x, lastY) | x <- newLocsRange ]
      Left  -> [ (lastX - x, lastY) | x <- newLocsRange ]
      Up    -> [ (lastX, lastY + y) | y <- newLocsRange ]
      Down  -> [ (lastX, lastY - y) | y <- newLocsRange ]
  in
    walkSeq remainingDirections (head newLocs) (Q.fromList newLocs) <> locs
walkSeq _ _ locs        = locs


walkCost :: Q.Seq Location -> Int
walkCost = length
-- walkCost :: Q.Seq Location -> Int
-- walkCost locs = foldl realCost [] locs
--   where
--     -- realCost item cost | trace (show item <> " " <> show cost) False = undefined
--     realCost visited item =
--       case Q.elemIndexL item locs of
--         Just i -> i
--         _      -> cost + 1

calcCost :: Q.Seq Location -> Location -> Int
calcCost path target | trace (show target) False = undefined
calcCost path target =
  let
    roughCost = Q.takeWhileR (\x -> x /= target) (path)
    realCost = walkCost roughCost
  in
    realCost

partTwo :: IO ()
partTwo = do
  f <- readFile "./src/DayThree/data2.txt"
  let parsed = fmap (parseString pairs mempty) (lines f)
      ((Success l):(Success r):_) = fmap (\x -> fmap (\ds -> walk ds (0, 0) mempty) x) parsed
      intersected = S.intersection l r
      ((Success lSeq):(Success rSeq):_) = fmap (\x -> fmap (\ds -> walkSeq ds (0, 0) $ Q.singleton (0, 0)) x) parsed

  let costl = fmap (calcCost lSeq) $ S.toList intersected
      costr = fmap (calcCost rSeq) $ S.toList intersected

  print $ costl
  print $ costr
  print $ map (\(a,b) -> a + b) $ zip costl costr

  pure ()
