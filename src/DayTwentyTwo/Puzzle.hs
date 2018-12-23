{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module DayTwentyTwo.Puzzle where

import Control.Lens

-- puzzle input
depth = 6969
target = 9796

-- test input
depth' = 510
target' = 10

data Kind = Rocky | Narrow | Wet

type X = Int
type Y = Int
data Region = Region X Y

--
-- geoindex (per region)
--

-- 0,0 geoindex: 0
-- target geoindex: 0

-- if the Y coordinate is 0,
-- its geoindex is X * 16807

-- if the X coordinate is 0,
-- its goeindex is Y * 48271

-- otherwise, the geoindex is
-- geoindex (X-1, Y) * geoindex (X, Y-1)

--
-- erosion level (per region)
--

-- (geoindex + cave system's depth) `mod` 20183
-- if ^ `mod` 3 is 0 -> Rocky
--              is 1 -> Wet
--              is 2 -> Narrow
type Target = Int
type Depth  = Int

geoindex t p b
  | y == 0 && x == 0 = 0
  | y == t && x == t = 0
  | y == 0 = x * 16807
  | x == 0 = y * 48271
  | otherwise = (b !! (y * t) + (x - 1)) * (b !! ((y - 1) * t) + x)
  where
    y = p `div` t
    x = p `mod` t

-- the width is known -- 10
-- so y = 11
genGeodex :: Int -> Int -> [Int] -> [Int]
genGeodex t p b
  | (t * t) == p = b
  | otherwise =
    let g = geoindex t p b
        newBoard = b <> [g]
    in genGeodex (t) (p + 1) newBoard

partOne = print "blup"
