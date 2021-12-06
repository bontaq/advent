module DaySix.Puzzle where

import Control.Lens
import Data.Functor.Foldable
import Data.List.Split

-----------
-- Model --
-----------

-- an infinite list of zeros to represent how many fish
-- are at which point in their lifecycle, each day we'll
-- shift it left one to move them closer to zero
fishOnDays = repeat 0

-----------
-- Parse --
-----------

fileLocation = "./src/DaySix/data.txt"

parse :: IO [Int]
parse = fmap read . splitOn "," <$> readFile fileLocation

-------------------------
-- Part One (And Two!) --
-------------------------

initialize [] model = model
initialize (fish:fishes) model =
  initialize fishes (model & element fish %~ (+) 1)

-- shift the infinite list left one
runDay (toSpawn:fishes) = fishes
        & element 8 %~ (+) toSpawn -- for the new spawning fish
        & element 6 %~ (+) toSpawn -- reseting the existing fish

-- using an anamorphism to create an infinite list of fish-states
-- going into the future (it just re-applies the last result as a
-- new argument, while collecting results)
runDays = ana (\fishes-> Cons fishes (runDay fishes))

partOneTwo = do
  fishes <- parse
  let start = initialize fishes fishOnDays
      ranDays = runDays start

  -- since this is an infinite list containing infinite lists,
  -- we take 81 days from the fish, clip them all by 10 (since no
  -- fish lifespan is greater than 10), then take the last and
  -- sum it

  -- part one
  print . sum . last $ fmap (take 10) (take 81 ranDays)
  -- part two
  print . sum . last $ fmap (take 10) (take 257 ranDays)

  -- idk why it's off by one
