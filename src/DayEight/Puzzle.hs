module DayEight.Puzzle where

import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace

location = "./src/DayEight/data.txt"

parse = fmap (splitOn "|") . lines <$> readFile location

--------------
-- Part One --
--------------

isDigit chars = case length chars of
  2 -> True -- Digit "1"
  4 -> True -- Digit "4"
  3 -> True -- Digit "7"
  7 -> True -- Digit "8"
  _ -> False

-- top = 7 && 8 ! 1
-- middle = 8 - 0
-- topRight = 6 // 9 && 1

-- 1, 4, 7, 8 are known
-- 6, 9, 0 have length 6
-- 5, 2, 3 have length 5

middle :: String -> [String] -> Char
-- middle three words | trace (show three <> " " <> show words) False = undefined
middle three words =
  let (longest:nextLongest:_) = reverse $ sortOn length words
      sixes = filter ((==) 6 . length) words
      one = findOne words
      four = findFour words
      zeroOrNine = filter (\w -> length (w `intersect` one) == 2) sixes
      zero = head $ filter (\w -> length (w `intersect` four) == 3) zeroOrNine
  in case longest \\ zero of
    [] -> error "here"
    [a] -> a

partOne = do
  lines <- parse

  let outputs = fmap (\[_, output] -> words output) lines

  print $ sum $ length <$> fmap (filter isDigit) outputs

findOne :: [String] -> String
findOne = head . sortOn length

-- four minus seven minus middle = upper left
findThree words =
  let
    one = findOne words

    fives = filter ((==) 5 . length) words
    three = head $ filter (\w -> length (w `intersect` one) == 2) fives
  in three

findFive words =
  let fives = filter ((==) 5 . length) words
  in head $ filter (\w -> length (w `intersect` upperLeft (findThree words) words) == 1) fives

findFour = head . filter ((==) 4 . length)
findSeven = head . filter ((==) 3 . length)
-- findEight = head . filter ((==) 7 . length)

upperLeft three words = (findFour words \\ findSeven words) \\ (middle three words:[])

toDigit :: [String] -> String -> Int
toDigit words chars = case length chars of
  2 -> 1-- Digit "1"
  4 -> 4-- Digit "4"
  3 -> 7-- Digit "7"
  7 -> 8-- Digit "8"
  _ ->
    let
        one = findOne words

        fives = filter ((==) 5 . length) words
        three = head $ filter (\w -> length (w `intersect` one) == 2) fives
        five = head $ filter (\w -> length (w `intersect` upperLeft three words) == 1) $ fives \\ [three]
        two = head $ filter (\w -> length (w `intersect` upperLeft three words) == 0) $ fives \\ [three]

        middle' = middle three words

        sixes = filter ((==) 6 . length) words
        zero = head $ filter (notElem middle') sixes
        six = head $ filter (\w -> length (w `intersect` one) == 1) $ sixes \\ [zero]
        nine = head $ filter (\w -> length (w `intersect` one) == 2) sixes \\ [zero]
    in
      if sort chars == sort zero then 0 else
        if sort chars == sort six then 6 else
          if sort chars == sort nine then 9 else
            if sort chars == sort three then 3 else
              if sort chars == sort five then 5 else
                if sort chars == sort two then 2 else error $ show [zero, one, two, three, five, six, nine]

runDecode (input, output) =
  let decoder = zip (fmap (toDigit input) input) input
      decoded = fmap (\input -> find (\(_, w) -> sort w == sort input) decoder) output
  in fmap (fst . fromJust) decoded

-- so zero is the one without the middle of length 6 words

testLine = words "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"
testLine2 =
  ( words "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec"
  , words "fcgedb cgb dgebacf gc"
  )
testLine3 =
  ( words "febag gefbc acfegbd fdega ba adbe fecdga ebagfd gcbfad gba"
  , words "ebda gabfced baed bfgec"
  )

partTwo = do
  lines <- parse

  let outputs = fmap (\[input, output] -> (words input, words output)) lines
      decoded =
        sum
        $ fmap (\x -> read x :: Int)
        $ fmap (foldr (<>) "")
        $ fmap (fmap show)
        $ fmap runDecode outputs

  print $ decoded
