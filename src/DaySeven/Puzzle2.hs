module DaySeven.Puzzle2 where

import Data.List (sort)
import Data.Map hiding (foldr, filter)
import Data.Maybe

location = "./src/DaySeven/real.txt"

countSizes :: [String] -> [String] -> Map String Int -> Map String Int
countSizes [] _ sizes = sizes
countSizes (line:lines) path sizes =
  case words line of
    ["$", "cd", "/"]  -> countSizes lines ["/"] sizes
    ["$", "cd", ".."] -> countSizes lines (init path) sizes
    ["$", "cd", to]   -> countSizes lines (path <> ["/" <> to]) sizes
    ["$", "ls"]       -> countSizes lines path sizes
    ["dir", _]        -> countSizes lines path sizes
    [size, name]      ->
      let pathsToUpdate = scanl1 (<>) path
          size' = read size :: Int
          newSizes =
            foldr (\path map ->
                     alter (\value ->
                              if isNothing value
                              then Just size'
                              else (+ size') <$> value)
                    path map)
                sizes pathsToUpdate
      in countSizes lines path newSizes

partOne = do
  text <- lines <$> readFile location

  let counts = countSizes text [] empty
  -- print $ filter (< 100000) $ fmap snd $ toList counts
  print $ sum $ filter (< 100000) $ fmap snd $ toList counts

  let amountToFree = (\y -> 30000000 - (70000000 - y)) $ head . reverse . sort $ fmap snd $ toList counts

  -- print amountToFree
  print $ head . sort $ filter (> amountToFree) $ fmap snd $ toList counts

-- 5469168 is the right answer
-- 25204323
-- 44795677
