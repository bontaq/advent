module DayFive where

import Data.List
import Data.List.Split
import Control.Lens
import Data.List.Lens

parseStack (a:b:c:d:rest) = [b] <> parseStack rest
parseStack (a:b:c:rest) = [b] <> parseStack rest
parseStack (a:b:rest) = [b] <> parseStack rest
parseStack [] = []

reallign :: [String] -> [String]
reallign = fmap (dropWhile (== ' ')) <$> transpose

parseStacks lines =
  let parsed = fmap parseStack lines
  in reallign $ take (length parsed - 1) parsed

parseMove :: String -> (Int, Int, Int)
parseMove line =
  let [_, amount, _, from, _, to] = splitOn " " line
  in (read amount, read from, read to)

parseMoves lines = fmap parseMove lines

move :: [String] -> (Int, Int, Int) -> [String]
move cargo (amount, from, to) =
  let
    initial = cargo !! (from - 1)
    -- for part 1
    crates = reverse $ take amount initial
    -- for part 2
    -- crates = take amount initial
    newCargo = set (ix (from - 1)) (drop amount initial) cargo
    target = cargo !! (to - 1)
  in set (ix (to - 1)) (crates <> target) newCargo

run :: [String] -> [(Int, Int, Int)] -> [String]
run cargo moves = foldl move cargo moves

parse = do
  raw <- lines <$> readFile "./src/DayFive/data.txt"

  let
    stacks = parseStacks $ takeWhile (/= "") raw
    moves = parseMoves $ drop 1 $ dropWhile (/= "") raw
    ran = run stacks moves

  print (fmap head ran)
  pure (stacks, moves)
