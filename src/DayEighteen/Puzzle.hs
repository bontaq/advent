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
  map (\row -> map (\c -> mkTile c) row) . tokenize
  where
    tokenize = filter ((> 0) . length) . lines

collectTile :: (Maybe a, b) -> Maybe (a, b)
collectTile ((Just t), c) = Just (t, c)
collectTile (Nothing, _)  = Nothing

collectTiles :: [(Maybe a, b)] -> [(a, b)]
collectTiles = catMaybes . map collectTile

getSurrounding :: Board -> (Int, Int) -> [(Tile, (Int, Int))]
getSurrounding board (x, y) =
  let coords = [ (0, 0), (1, 0), (2, 0)
               , (0, 1),         (2, 1)
               , (0, 2), (1, 2), (2, 2) ]
      adjustedCoords = map (\(x', y') -> (x' + x - 1, y' + y - 1)) coords
  in collectTiles $ map (\(x, y) -> (board ^? ix x . ix y, (x, y))) adjustedCoords

getByTile :: Tile -> [(Tile, b)] -> [(Tile, b)]
getByTile t = filter ((==) t . fst)

getNextTile :: Tile -> [(Tile, (Int, Int))] -> Tile
getNextTile Open       surrounding
  | fs >= 3   = Forest
  | otherwise = Open
  where fs = length $ getByTile Forest surrounding

getNextTile Forest     surrounding
  | ls >= 3   = Lumberyard
  | otherwise = Forest
  where ls = length $ getByTile Lumberyard surrounding

getNextTile Lumberyard surrounding
  | ls > 0 && fs > 0 = Lumberyard
  | otherwise = Open
  where ls = length $ getByTile Lumberyard surrounding
        fs = length $ getByTile Forest surrounding

groupByX :: Eq a => [(a, b)] -> [[(a, b)]]
groupByX = groupBy (\(x, _) (x', _) -> x == x')

withCoordinates :: Board -> [[(Tile, (Int, Int))]]
withCoordinates board =
  let x = length board
      y = length (board !! 0)
      coords = groupByX $ liftA2 (,) ([0..x]) ([0..y])
  in zipWith zip board coords

runMinute :: Board -> Board
runMinute board =
  let withCoords = withCoordinates board
  in board

partOne = print "sup"
