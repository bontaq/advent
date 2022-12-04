module DayThirteen.Puzzle where

import Data.List.Split
import Data.List
import Control.Lens

fileLocation = "./src/DayThirteen/data.txt"

handleCoords :: [String] -> [(Int, Int)]
handleCoords str = (\[x, y] -> (read x, read y)) <$> fmap (splitOn ",") str

handleFold str =
  let [value] = drop 2 . words $ str
      [axis, point] = splitOn "=" value
  in (axis, read point :: Int)

handleFolds = fmap handleFold

parse :: String -> ([(Int, Int)], [(String, Int)])
parse str =
  let [coords, folds] = splitOn [""] . lines $ str
  in (handleCoords coords, handleFolds folds)

foldY fold = fmap handle
  where
    handle (x, y) = if y > fold then (x, fold - (y - fold)) else (x, y)

foldX fold = fmap handle
  where
    handle (x, y) = if x > fold then (fold - (x - fold), y) else (x, y)

run points commands = foldl handle points commands
  where
    handle points ("x", fold) = foldX fold points
    handle points ("y", fold) = foldY fold points

partOne = do
  (coords, folds) <- parse <$> readFile fileLocation

  pure $ nub $ run coords folds

setPoints map points = foldr setPoint map points
  where
    setPoint (x, y) map = map & element y . element x %~ (+ 1)

partTwo = do
  (coords, folds) <- parse <$> readFile fileLocation

  let field = [x | y <- [0..10], let x = take 50 (repeat 0)]

  mapM print $ setPoints field $ nub $ run coords folds
