{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module DayTwentyTwo.Puzzle where

-- import Control.Lens
import Data.Sequence
import Data.Foldable (toList)

-- puzzle input
depth = 6969
target = (9, 796)

-- test input
depth' = 510
target' = 10

data Kind = Rocky | Narrow | Wet
instance Show Kind where
  show Rocky  = "."
  show Wet    = "="
  show Narrow = "|"

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
type Width  = Int

erode n d = (d + n) `mod` 20183

geoindex :: Int -> Int -> Int -> Seq Int -> Int
geoindex width point d b
  | y == 0 && x == 0 = 0
  | y == (width - 1) && x == (width - 1) = 0
  | y == 0 = x * 16807
  | x == 0 = y * 48271
  | otherwise =
    let x' = (index b ((y * width) + (x - 1)))
        y' = (index b (((y - 1) * width) + x))
    in (erode x' d) * (erode y' d)
  where
    y = point `div` width
    x = point `mod` width

genGeodex :: Width -> Int -> Depth -> Seq Int -> [Int]
genGeodex width p d b
  | (width * width) == p = []
  | otherwise =
    let g = geoindex width p d b
        newBoard = b |> g
    in g : genGeodex width (p + 1) d newBoard

genErosion :: Depth -> [Int] -> [Kind]
genErosion _     []     = []
genErosion depth (i:is) =
  let preq = ((depth + i) `mod` 20183)
  in
    case preq `mod` 3 of
      0 -> Rocky  : genErosion depth is
      1 -> Wet    : genErosion depth is
      2 -> Narrow : genErosion depth is

findDanger :: [Kind] -> Int
findDanger ks =
  let danger k = case k of
        Rocky  -> 0
        Wet    -> 1
        Narrow -> 2
  in
    foldr ((+) . danger) 0 ks

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (Prelude.take n l) : (group n (Prelude.drop n l))
  | otherwise = error "Negative n"

-- wrong: 95970117

partOne = do
  print . show $
    findDanger
    $ genErosion (depth')
    $ genGeodex (target' + 1) 0 (depth') mempty
