module DayNine.Puzzle where

import Data.Maybe
import Data.Bifunctor
import Data.List
import Control.Lens
import Debug.Trace

fileLocation = "./src/DayNine/data.txt"

parse :: IO [[Int]]
parse = fmap (fmap (\c -> read [c])) . lines <$> readFile fileLocation

--------------
-- Part One --
--------------

getLowPoint :: (Int, Int) -> [[Int]] -> Maybe Int
getLowPoint (x, y) map =
  let surrounding =
        [ map ^? ix (y - 1) . ix x
        , map ^? ix (y + 1) . ix x
        , map ^? ix y . ix (x - 1)
        , map ^? ix y . ix (x + 1)
        ]
      value = map ^?! ix y . ix x
  in if value < minimum (catMaybes surrounding) then Just value else Nothing

partOne = do
  map <- parse

  let lowsMap =
        fmap (\(y, row) -> fmap (\(x, _) -> getLowPoint (x, y) map) (zip [0..] row)) (zip [0..] map)

  print $ sum $ fmap (+ 1) $ catMaybes $ concat lowsMap

--------------
-- Part Two --
--------------

collectBasin :: (Int, Int) -> [(Int, Int)] -> [[Int]] -> [(Int, Int)]
-- collectBasin a b c | trace (show a <> " " <> show b) False = undefined
collectBasin (x, y) seen map =
  let
    points =
      [ (x - 1, y)
      , (x + 1, y)
      , (x, y - 1)
      , (x, y + 1)
      , (x, y)
      ]
    newPoints = filter (`notElem` seen) points
    getPoint (x, y) = map ^? ix y . ix x
    values = zip newPoints (fmap getPoint newPoints)
    noTops = second fromJust
      <$> filter (\(_, newPoint) -> isJust newPoint && newPoint /= Just 9) values
    newSeenPoints = fmap fst noTops
  in foldr (\point seen -> seen <> collectBasin point seen map) (nub $ seen <> newSeenPoints) newSeenPoints

partTwo = do
  map <- parse

  let lowsMap =
        fmap (\(y, row) -> fmap (\(x, _) -> (getLowPoint (x, y) map, (x, y))) (zip [0..] row)) (zip [0..] map)
      onlyLows = filter (\(maybeLow, _) -> isJust maybeLow) $ concat lowsMap
      basins = fmap (\point -> collectBasin point [] map) (fmap snd onlyLows)

  -- print $ length onlyLows

  print $ product $ take 3 $ reverse . sort $ fmap (length . nub) basins
