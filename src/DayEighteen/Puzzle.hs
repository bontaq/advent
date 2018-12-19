{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module DayEighteen.Puzzle where

import Text.RawString.QQ (r)
import Control.Applicative
import Control.Lens
import Data.Maybe (catMaybes)
import Data.List (groupBy)


--    An open acre will become filled with trees if three or more
--      adjacent acres contained trees. Otherwise, nothing happens.

--    An acre filled with trees will become a lumberyard if three
--      or more adjacent acres were lumberyards. Otherwise, nothing happens.

--    An acre containing a lumberyard will remain a lumberyard if
--      it was adjacent to at least one other lumberyard and at
--      least one acre containing trees. Otherwise, it becomes open.

start = [r|
.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.
|]

data Tile = Forest | Lumberyard | Open
          deriving Eq
instance Show Tile where
  show Open       = "."
  show Forest     = "|"
  show Lumberyard = "#"

type Board = [[Tile]]

showBoard :: Board -> IO ()
showBoard = mapM_ (print . (filter (/= ',')) . show)

mkTile :: Char -> Tile
mkTile '.' = Open
mkTile '#' = Lumberyard
mkTile '|' = Forest

mkBoard :: [Char] -> [[Tile]]
mkBoard =
  id $! map (\row -> map (\c -> mkTile c) row) . tokenize
  where
    tokenize = filter ((> 0) . length) . lines

collectTile :: (Maybe a, b) -> Maybe (a, b)
collectTile ((Just t), c) = Just (t, c)
collectTile (Nothing, _)  = Nothing

collectTiles :: [(Maybe a, b)] -> [(a, b)]
collectTiles = id $! catMaybes . map collectTile

getSurrounding :: (Int, Int) -> Board -> [(Tile, (Int, Int))]
getSurrounding (x, y) board =
  let coords = [ (0, 0), (1, 0), (2, 0)
               , (0, 1),         (2, 1)
               , (0, 2), (1, 2), (2, 2) ]
      adjustedCoords = map (\(x', y') -> (x' + x - 1, y' + y - 1)) coords
  in id $! collectTiles $ map (\(x, y) -> (board ^? ix x . ix y, (x, y))) adjustedCoords

getByTile :: Tile -> [(Tile, b)] -> [(Tile, b)]
getByTile t = id $! filter ((==) t . fst)

getNextTile :: Tile -> [(Tile, (Int, Int))] -> Tile
getNextTile Open       surrounding
  | fs >= 3   = Forest
  | otherwise = Open
  where !fs = length $ getByTile Forest surrounding

getNextTile Forest     surrounding
  | ls >= 3   = Lumberyard
  | otherwise = Forest
  where !ls = length $ getByTile Lumberyard surrounding

getNextTile Lumberyard surrounding
  | ls > 0 && fs > 0 = Lumberyard
  | otherwise = Open
  where !ls = length $ getByTile Lumberyard surrounding
        !fs = length $ getByTile Forest surrounding

groupByX :: Eq a => [(a, b)] -> [[(a, b)]]
groupByX = id $! groupBy (\(x, _) (x', _) -> x == x')

withCoordinates :: Board -> [[(Tile, (Int, Int))]]
withCoordinates !board =
  let !x = length board
      !y = length (board !! 0)
      !coords = groupByX $ liftA2 (,) ([0..x]) ([0..y])
  in id $! zipWith zip board coords

runMinute :: Board -> Board
runMinute !board =
  let !withCoords = withCoordinates board
  in
    id $! map (\row ->
          id $! map
          (\(tile, (x, y)) ->
            id $! getNextTile tile $ getSurrounding (x, y) board)
          row)
    withCoords

runMinutes :: Int -> Board -> Board
runMinutes 0 !b = b
runMinutes !i !b =
  let !newBoard = runMinute b
      !newI = i - 1
  in runMinutes newI newBoard

partOne = do
  board' <- readFile "./src/DayEighteen/Data.txt"
  let ansBoard = runMinutes 10 (mkBoard board')
      woodCount = length $ filter (== Forest) $ concat ansBoard
      lumberCount = length $ filter (== Lumberyard) $ concat ansBoard
  print woodCount
  print lumberCount
  print (woodCount * lumberCount)
  -- showBoard $ runMinutes 10 (mkBoard board')

partTwo = do
  board' <- readFile "./src/DayEighteen/Data.txt"
  let !ansBoard = runMinutes 1000000000 (mkBoard board')
      woodCount = length $ filter (== Forest) $ concat ansBoard
      lumberCount = length $ filter (== Lumberyard) $ concat ansBoard
  print woodCount
  print lumberCount
  print (woodCount * lumberCount)
