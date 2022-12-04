{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module DayEleven.Puzzle where

import Data.List
import Data.Maybe
import Control.Lens

parse :: IO [[Int]]
parse = fmap (fmap (\c -> read [c])) . lines <$> readFile "./src/DayEleven/data.txt"

type Board = [[(Int, Bool)]]

surrounding (x, y) =
  [ (x - 1, y)
  , (x + 1, y)
  , (x, y + 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  , (x + 1, y + 1)
  , (x - 1, y - 1)
  , (x - 1, y + 1)
  ]

flashPoint :: (Int, Int) -> Board -> Board
flashPoint (x, y) map =
  map & element y . element x %~ increment
  where increment (point, True) = (point, True)
        increment (point, False) = (point + 1, False)

runFlash (x, y) map =
  let flashedMap = foldr flashPoint map (surrounding (x, y))
  in flashedMap & element y . element x %~ const (0, True)

-- setFlashed (x, y) map =
  -- map & element x . element y %~ (\(point, fired) -> (0, True))

getPoint :: (Int, Int) -> Board -> (Int, Bool)
getPoint (x, y) map = map ^?! ix y . ix x

shouldFlash point map =
  let (power, flashed) = getPoint point map
  in power > 9 && not flashed

runStep map =
  let
    coords = concat $ zipWith (\row y -> zip [0..length row - 1] (repeat y)) map [0..]
    handleFlash point map =
      if shouldFlash point map
      then runFlash point map
      else map
  in
    foldr handleFlash map coords

shouldContinue = isJust . find (\(point, flashed) -> point > 9 && not flashed) . concat

loop map =
  let newMap = runStep map
  in if shouldContinue newMap then loop newMap else newMap

run :: (Int, Board) -> (Int, Board)
run (count, map) =
  let plussed = over (traverse . traverse) (\(power, flashed) -> (power + 1, False)) map
      ran = loop plussed
  in ((+) count . length . filter ((==) 0 . fst) . concat $ ran, ran)

partOne = do
  octopi <- parse

  let mapWithFlash = (fmap . fmap) (, False) octopi
      futures = iterate run (0, mapWithFlash)
  print $ futures !! 100

-- run' :: (Bool, Board) -> (Bool, Board)
run' (continue, map) =
  let plussed = over (traverse . traverse) (\(power, flashed) -> (power + 1, False)) map
      ran = loop plussed
  in (all ((==) 0 . fst) . concat $ ran, ran)

partTwo = do
  octopi <- parse

  let mapWithFlash = (fmap . fmap) (, False) octopi
      futures = iterate run' (False, mapWithFlash)

  print $ findIndex (\(a, _) -> a) futures
