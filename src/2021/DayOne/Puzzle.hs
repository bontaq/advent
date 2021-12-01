module DayOne.Puzzle where

parseInt :: String -> Int
parseInt = read

getNumbers = fmap parseInt <$> lines <$> readFile "./src/2021/DayOne/data.txt"

countIncreases' :: Int -> [Int] -> Int
countIncreases' count (past:current:rest) =
  let
    newCount = if past < current then count + 1 else count
  in
    countIncreases' newCount (current:rest)

countIncreases' count _ = count

countIncreases = countIncreases' 0

partOne = print =<< countIncreases <$> getNumbers

window (a:b:c:rest) = [a,b,c] : window (b:c:rest)
window _ = []

partTwo = print =<< countIncreases <$> fmap sum <$> window <$> getNumbers
