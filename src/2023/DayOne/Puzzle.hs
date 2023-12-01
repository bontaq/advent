{-# LANGUAGE QuasiQuotes #-}
-- |

module DayOne.Puzzle where

import Text.RawString.QQ (r)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Applicative (Alternative(empty))
import Data.List.Extra

test = [r|
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|]

clean = filter (not . null)

prepass xs =
  replace "nine" "nine9nine" $
  replace "eight" "eight8eight" $
  replace "seven" "seven7seven" $
  replace "six" "six6six" $
  replace "five" "five5five" $
  replace "four" "four4four" $
  replace "three" "three3three" $
  replace "two" "two2two" $
  replace "one" "one1one" xs

collectNumbers :: [Char] -> [Int]
collectNumbers = mapMaybe (\c -> readMaybe [c])

getDigits :: [b] -> (b, b)
getDigits numbers = (head numbers, last numbers)

toFinalNumber :: (Int, Int) -> Int
toFinalNumber (a, b) = read $ show a <> show b

location = "./src/2023/DayOne/data.txt"

main = do
  input <- readFile location

  let rows = collectNumbers <$> lines (prepass input)
      digits = fmap getDigits (clean rows)
      finalNumbers = fmap toFinalNumber digits

  print digits
  print $ sum finalNumbers
