-- |

module DayNine.Puzzle where

import Data.List
import Debug.Trace

location = "./src/DayNine/test2.txt"

parseLine line =
  let [direction, amount] = words line
  in (direction, read amount :: Int)

input = do
  lines' <- lines <$> readFile location

  pure (fmap parseLine lines')

type X = Int
type Y = Int
type Point = (X, Y)
type Direction = (String, Int)

move :: Point -> Direction -> Point
move (startX, startY) direction = case direction of
  ("R", amount) -> (startX + amount, startY)
  ("L", amount) -> (startX - amount, startY)
  ("U", amount) -> (startX, startY + amount)
  ("D", amount) -> (startX, startY - amount)

getNewPoint (x, y) (x', y')
  | x' /= x && y' /= y =
    let newX = fst $ getNewPoint (x, y') (x', y')
        newY = snd $ getNewPoint (x', y) (x', y')
    in (newX, newY)
  | x' < x = (x' + 1, y')
  | x' > x = (x' - 1, y')
  | y' < y = (x', y' + 1)
  | y' > y = (x', y' - 1)

follow :: Point -> Point -> [Point]
follow (headX, headY) (tailX, tailY) =
  let xDistance = abs $ headX - tailX
      yDistance = abs $ headY - tailY
  in if xDistance >= 2 || yDistance >= 2
     then
       let newPoint = getNewPoint (headX, headY) (tailX, tailY)
       in [newPoint] <> follow (headX, headY) newPoint
     else []

loop :: Point -> Point -> [Direction] -> [Point]
loop _       tailPos []         = []
loop headPos tailPos (row:rows) =
  let moved = move headPos row
      visited = follow moved tailPos
      tailPos' = if not (null visited) then last visited else tailPos
  in visited <> loop moved tailPos' rows

partOne = do
  input' <- input

  let visited = loop (0, 0) (0, 0) input'

  -- print visited
  -- mapM_ print (reverse $ linz $ drawPoints visited)
  print $ length (nub visited)

linz []  = []
linz arr = [take 51 arr] <> linz (drop 51 arr)

drawPoints :: [Point] -> [Char]
drawPoints points = [z | y <- [-25..25], x <- [-25..25], let z = found (x, y) ]
  where found point = case find (point ==) points of
          (Just _) -> 'X'
          _        -> '.'

test [] = []
test [a] = []
test (a:b:rest) = [a, b] <> test (b:rest)

inner :: [Point] -> [[Point]]
inner [] = []
inner [a] = [[a]]
inner (a:b:rest) =
  let locations = follow a b
      newLocation = if null locations then b else last locations
  in [locations <> [a]] <> inner (newLocation:rest)

loop' :: [Point] -> [Direction] -> [[Point]]
-- loop' points directions | trace (show points) False = undefined
loop' _ [] = []
loop' (top:rest) (row:rows) =
  let head = move top row
      moves = inner (head:rest)
      newTail = moves !! (length moves - 2)
      newLocations = fmap last moves
  in [newTail] <> loop' newLocations rows

{- Now there are 10 knots -}
partTwo = do
  input' <- input

  let
    points = replicate 11 (0, 0)
    looped = loop' points input'

  print looped
  print (length $ nub (concat looped))

  mapM_ print (reverse $ linz $ drawPoints (concat looped))

  pure ()

-- 2739 too high
-- 2738 too high
