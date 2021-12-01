module DayOne.Puzzle where

parseInt :: String -> Int
parseInt = read

numbers = fmap parseInt <$> lines <$> readFile "./src/2021/DayOne/data.txt"

countIncreases numbers =
  subtract 1 -- the first one's always true because of the leading 0
  $ length
  $ filter (\(current, past) -> current > past)
  $ (zip numbers (0:numbers))

partOne = print =<< countIncreases <$> numbers

window (a:b:c:rest) = [a,b,c] : window (b:c:rest)
window _ = []

partTwo = do
  numbers' <- fmap sum <$> window <$> numbers
  print $ countIncreases numbers'
