{-# LANGUAGE QuasiQuotes #-}

module DayFifteen.Puzzle where

import Prelude hiding (length, filter, zipWith, zip, head)
import qualified Prelude as P
import Text.RawString.QQ (r)
import Control.Applicative
import Data.List (groupBy)
import Data.Sequence
import Data.Foldable (toList)
-- import Control.Lens


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
          deriving Eq
instance Show Tile where
  show Wall   = "#"
  show Elf    = "E"
  show Goblin = "G"
  show Open   = "."

type Board = Seq (Seq Tile)

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

mkBoard :: String -> Board
mkBoard s =
  let arr = (map . map $ mkTile) . (P.filter ((> 0) . P.length)) . lines
  in fromList $ map fromList $ arr s

showBoard :: Board -> IO ()
showBoard b = mapM_ (print . (P.filter (/= ',')) . show) $ toList b

groupByX :: Eq a => [(a, b)] -> [[(a, b)]]
groupByX = groupBy (\(x, _) (x', _) -> x == x')

withCoordinates :: Board -> Seq (Seq (Tile, (Int, Int)))
withCoordinates board =
  let x = length board
      y = length (index board 0)
      coords = groupByX $ liftA2 (,) ([0..x]) ([0..y])
      coords' = fromList $ fmap fromList coords
  in zipWith zip board coords'

isCharacter :: Tile -> Bool
isCharacter Goblin = True
isCharacter Elf    = True
isCharacter _      = False

characterLocations :: Board -> Seq (Tile, (Int, Int))
characterLocations board =
  let board' = withCoordinates board
  in filter (isCharacter . fst) (foldr (><) mempty board')

enemy :: Tile -> Tile
enemy Goblin = Elf
enemy Elf    = Goblin

readSpot :: Int -> Int -> Board -> Maybe Tile
readSpot x y board =
  case (board !? y) of
    Nothing   -> Nothing
    Just yRow -> yRow !? x

getSurrounding :: Int -> Int -> Board -> [[Maybe Tile]]
getSurrounding x y board =
  let
    xRange = [x-1..x+1]
    yRange = [y-1..y+1]
    spots = map (\y -> map (\x -> readSpot x y board) xRange) yRange
  in spots

openMoves :: (Tile, (Int, Int)) -> Board -> (Int, Int)
openMoves (_, (x, y)) = undefined

runTurn :: (Tile, (Int, Int)) -> Board -> a
runTurn character board =
  let locs = characterLocations board
      (meKind, _) = character
      enemies = filter (\(kind, _) -> kind == enemy meKind) locs
      enemiesOpen' = fmap (flip openMoves board) enemies
  in undefined

-- all actions are completed and written back to the board
-- each turn
turn :: Board -> Board
turn board =
  let locs = characterLocations board
  in foldr runTurn board locs

partOne = do
  res <- readFile "src/DayFifteen/Data.txt"
  print "hoo boy"

-- set the first index of a set to "what"
-- a & ix 0 .~ "what"
-- view the first index of a set
-- a ^. ix 0
