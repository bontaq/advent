module DayThree.Puzzle where

import Control.Lens

type Field = [[Char]]

genField :: IO Field
genField =
  fmap cycle  -- make X axis infinitely repeating
  <$> lines   -- breaks by \n
  <$> readFile "./src/DayThree/data.txt"

type Y = Int
type X = Int
type Point = (X, Y)

genPoints :: X -> Y -> [Point]
genPoints x y = zip (iterate (+ x) 0) (iterate (+ y) 0)  -- also infinite lol

collect :: [Point] -> Field -> [Char]
collect []              _     = []
collect ((x, y):points) field =
  case field ^? element x . element y of
    Just char -> char : collect points field
    Nothing   -> []

treeFilter :: [Char] -> [Char]
treeFilter = filter (== '#')

partOne = do
  field <- genField
  print $ length $ treeFilter $ collect (genPoints 1 3) field

partTwo = do
  field <- genField
  print
    $ product
    $ fmap
        (\(x, y) -> length $ treeFilter $ collect (genPoints x y) field)
        [ (1, 1)
        , (1, 3)
        , (1, 5)
        , (1, 7)
        , (2, 1)
        ]
