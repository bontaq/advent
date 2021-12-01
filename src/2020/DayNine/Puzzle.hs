module DayNine.Puzzle where

genSums a = [ x + y | x <- a
                    , y <- a
                    , x /= y ]

findInvalid :: [Integer] -> [Integer] -> Integer
findInvalid window (check:remain) =
  let valid = check `elem` genSums window
  in
    if not valid
    then check
    else findInvalid ((drop 1 window) <> [check]) remain

partOne = do
  nums <- (fmap (\s -> read s :: Integer)) <$> lines <$> readFile "./src/DayNine/data.txt"
  print $ findInvalid (take 25 nums) (drop 25 nums)
  pure ()

test =
  [ "35"
  , "20"
  , "15"
  , "25"
  , "47"
  , "40"
  , "62"
  , "55"
  , "65"
  , "95"
  , "102"
  , "117"
  , "150"
  , "182"
  , "127"
  , "219"
  , "299"
  , "277"
  , "309"
  , "576"
  ]

test' =
  let nums = fmap (\s -> read s :: Integer) test
  in
    findInvalid (take 5 nums) (drop 5 nums)
