{-# LANGUAGE QuasiQuotes #-}

module DayEighteen.Puzzle where

import Text.RawString.QQ (r)
import Control.Lens

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
instance Show Tile where
  show Open       = "."
  show Forest     = "|"
  show Lumberyard = "#"

type Board = [[Tile]]

showBoard :: Board -> IO ()
showBoard = mapM_ (print . (filter (/= ',')) . show)

mkTile '.' = Open
mkTile '#' = Lumberyard
mkTile '|' = Forest

mkBoard :: [Char] -> [[Tile]]
mkBoard =
  map (\row -> map (\c -> mkTile c) row) . tokenize
  where
    tokenize = filter ((> 0) . length) . lines

partOne = print "sup"
