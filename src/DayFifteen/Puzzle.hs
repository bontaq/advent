{-# LANGUAGE QuasiQuotes #-}

module DayFifteen.Puzzle where

import           Text.RawString.QQ (r)

-- Initially:
-- #######
-- #.G...#   G(200)
-- #...EG#   E(200), G(200)
-- #.#.#G#   G(200)
-- #..G#E#   G(200), E(200)
-- #.....#
-- #######
--
-- After 1 round:
-- #######
-- #..G..#   G(200)
-- #...EG#   E(197), G(197)
-- #.#G#G#   G(200), G(197)
-- #...#E#   E(197)
-- #.....#
-- #######
--
-- After 2 rounds:
-- #######
-- #...G.#   G(200)
-- #..GEG#   G(200), E(188), G(194)
-- #.#.#G#   G(194)
-- #...#E#   E(194)
-- #.....#
-- #######

data Tile = Wall | Elf | Goblin | Open
instance Show Tile where
  show Wall   = "#"
  show Elf    = "E"
  show Goblin = "G"
  show Open   = "."

type Board = [[Tile]]

-- turn order determined by reading order
-- turns:
--
-- unit identifies all open squares that are
-- in range of targets
--
-- if there are no open squares in range of
-- a target, turn ends
--
-- to move, take all the squares in range of
-- a target.  determine which square could be
-- reached in the fewest steps
--
-- if there is a tie, break with reading order

-- alright, that's enough to get started

start = [r|
#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######
|]

mkTile :: Char -> Tile
mkTile '.' = Open
mkTile 'G' = Goblin
mkTile 'E' = Elf
mkTile '#' = Wall


-- mkBoard :: [Char] -> Board
mkBoard =
  (map . map $ mkTile) . (filter ((> 0) . length)) . lines

showBoard :: Board -> IO ()
showBoard = mapM_ (print . (filter (/= ',')) . show)

partOne = do
  res <- readFile "src/DayFifteen/Data.txt"
  print "hoo boy"
