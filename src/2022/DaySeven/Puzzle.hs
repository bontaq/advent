-- |

module DaySeven.Puzzle where

import Data.List

testLocation = "./src/DaySeven/real.txt"

parse = lines <$> readFile testLocation

buildFiles [] _ = []
buildFiles (line:output) dir =
  case line of
    ('$':rest) -> case words rest of
        ["cd", location] ->
          case location of
            ".." -> buildFiles output (init dir)
            "/"  -> buildFiles output ["/"]
            _    -> buildFiles output (dir <> [location])
        ["ls"] -> buildFiles output dir
    fileInfo -> case words fileInfo of
      ["dir", _]   -> buildFiles output dir
      [size, name] -> [(dir, read size :: Int, name)] <> buildFiles output dir

getSize :: [String] -> [([String], Int, String)] -> Int
getSize dir dirs =
  sum $ getSize <$> filter (isPrefixOf dir . getDir) dirs
  where
    getSize (_, size, _) = size
    getDir  (dir,  _, _) = dir

getSizes files =
  let uniqDirs = nub $ fmap (\(it, _, _) -> it) files
  in fmap (\dir -> (dir, getSize dir files)) uniqDirs

partOne = do
  raw <- parse
  let
    files = buildFiles raw []
    sizes = getSizes files
    limited = filter (\(_, size) -> size < 100000) sizes
    total = sum $ fmap snd $ limited

  -- mapM_ print limited
  -- mapM_ print $ sortOn (\(it, _, _) -> it) files
  mapM_ print $ files
  print total

-- 1265478 is too low
-- 1391690 is the answer
