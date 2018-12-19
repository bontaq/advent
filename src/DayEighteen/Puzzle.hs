{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module DayEighteen.Puzzle where

import Prelude hiding (filter, length, zip, zipWith, concat)
import Text.RawString.QQ (r)
import Control.Applicative
import Control.DeepSeq (force)
import Data.Maybe (catMaybes, fromJust)
import Data.List (groupBy)
import Data.Sequence
import Control.Exception.Base
import Data.Foldable (toList, concat)


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

type Board = Seq (Seq Tile)

-- showBoard :: Board -> IO ()
-- showBoard = mapM_ (print . (filter (/= ',')) . show)

mkTile :: Char -> Tile
mkTile '.' = Open
mkTile '#' = Lumberyard
mkTile '|' = Forest

mkBoard :: [Char] -> Board
mkBoard raw =
  let
    raw' = fromList $ map fromList $ lines raw
    tokenized = filter ((> 0) . length) $ raw'
  in
    fromList
    $ map (\row -> fromList $ map (\c -> mkTile c) (toList row))
    $ toList tokenized

unpackMaybes :: Seq (Maybe Tile, b) -> Seq (Tile, b)
unpackMaybes = foldr (\(Just a, b) acc -> (a, b) <| acc) mempty . filter ((/= Nothing) . fst)

coordAdjustments :: Seq (Int, Int)
coordAdjustments = fromList [ (0, 0), (1, 0), (2, 0)
                            , (0, 1),         (2, 1)
                            , (0, 2), (1, 2), (2, 2) ]

adjustedCoords :: (Num a, Num b) => a -> b -> Seq (a, b) -> Seq (a, b)
adjustedCoords x y = fmap (\(x', y') -> (  x' + x - 1
                                        ,  y' + y - 1 ))

getSurrounding :: (Int, Int) -> Board -> Seq (Tile, (Int, Int))
getSurrounding (x, y) board =
  let coords = coordAdjustments
      adjustedCoords' = adjustedCoords x y coords
  in
    unpackMaybes
    $ fmap (\(x, y) ->
             ((board !? x) >>= (flip (!?) y), (x, y))) adjustedCoords'

getByTile :: Eq a => a -> Seq (a, b) -> Seq (a, b)
getByTile !t = filter ((==) t . fst)

getNextTile :: Tile -> Seq (Tile, (Int, Int)) -> Tile
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

coords = (fromList . map fromList) $ groupByX $ liftA2 (,) ([0..50]) ([0..50])

withCoordinates :: Board -> Seq (Seq (Tile, (Int, Int)))
withCoordinates board =
  mapWithIndex (\y row -> mapWithIndex (\x t -> (t, (y, x))) row) board

runMinute :: Board -> Board
runMinute board =
  let withCoords = force withCoordinates board
  in
    fmap (\row -> fmap
           (\(tile, (x, y)) -> getNextTile tile $ getSurrounding (x, y) board)
           row)
    withCoords

runMinutes :: Int -> Board -> IO ()
runMinutes 0 b = do
  let ansBoard = b
      woodCount = length $ filter (== Forest) $ foldr (><) mempty ansBoard
      lumberCount = force length $ filter (== Lumberyard) $ foldr (><) mempty ansBoard
  print $ (woodCount * lumberCount)
runMinutes i b = do
  let ansBoard = runMinute b
      lumberCount = length $ filter (== Lumberyard) $ foldr (><) mempty ansBoard
  _ <- evaluate lumberCount
  runMinutes (i - 1) ansBoard

partOne = do
  board' <- readFile "./src/DayEighteen/Data.txt"
  runMinutes 10 (mkBoard board')
partTwo = do
  board' <- readFile "./src/DayEighteen/Data.txt"
  runMinutes 1000000000 (mkBoard board')
