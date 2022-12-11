-- |

module DayEight.Puzzle where

-- import Data.Set
import Data.List

location = "./src/DayEight/real.txt"

type Y = Int
type X = Int
type Tree = ((X, Y), Int)

toGrid :: IO [[Tree]]
toGrid = do
  rows <- lines <$> readFile location
  let
    parsed = fmap (\c -> read [c] :: Int) <$> rows
    withCoords =
      fmap (\(row, y) ->
              fmap (\(height, x) -> ((x, y), height)) (zip row [0..]))
      (zip parsed [0..])

  pure withCoords

getEdges :: [[Tree]] -> [Tree]
getEdges trees =
  head trees <> last trees <> fmap head trees <> fmap last trees

walk :: Int -> [Tree] -> [Tree]
walk _      [] = []
walk height (tree:trees)
  | snd tree > height = tree : walk (snd tree) trees
  | otherwise = walk height trees

test = do
  grid <- toGrid
  print $ look ((2, 0), 3) (concat grid)

look :: Tree -> [Tree] -> [Tree]
look tree@(cursor, height) trees =
  let
    lookLeft = filter (\((x, y), _) -> x > fst cursor && y == snd cursor) trees
    lookRight  = reverse $ filter (\((x, y), _) -> x < fst cursor && y == snd cursor) trees
    lookUp = reverse $ filter (\((x, y), _) -> y < snd cursor && x == fst cursor) trees
    lookDown = filter (\((x, y), _) -> y > snd cursor && x == fst cursor) trees
  in
    [tree]
    <> walk height lookLeft
    <> walk height lookRight
    <> walk height lookUp
    <> walk height lookDown

partOne = do
  grid <- toGrid

  let
    toCheck = nub $ getEdges grid
    visible = fmap (\tree -> look tree (concat grid)) toCheck

  -- print $ length visible
  -- mapM_ print $ visible
  print $ length $ nub (concat visible)

walk' :: Int -> [Tree] -> [Tree]
walk' _      [] = []
walk' height (tree:trees)
  | snd tree >= height = [tree]
  | otherwise = tree : walk' height trees

look' :: Tree -> [Tree] -> [[Tree]]
look' tree@(cursor, height) trees =
  let
    lookLeft = filter (\((x, y), _) -> x > fst cursor && y == snd cursor) trees
    lookRight  = reverse $ filter (\((x, y), _) -> x < fst cursor && y == snd cursor) trees
    lookUp = reverse $ filter (\((x, y), _) -> y < snd cursor && x == fst cursor) trees
    lookDown = filter (\((x, y), _) -> y > snd cursor && x == fst cursor) trees
  in
    []
    <> [walk' height lookLeft]
    <> [walk' height lookRight]
    <> [walk' height lookUp]
    <> [walk' height lookDown]

score :: [[a]] -> Int
score = product . fmap length

partTwo = do
  grid <- toGrid

  let ans = (look' ((2, 1), 5) (concat grid))
      scores = fmap (\tree -> score $ look' tree (concat grid)) (concat grid)

  print (maximum scores)
  print (score ans)
