{-# LANGUAGE FlexibleContexts #-}
module Puzzle where

import Data.List
import Data.Maybe

file = lines <$> readFile "./src/DayThree/data.txt"

split line = splitAt (length line `div` 2) line

scores = zip [1..] (['a'..'z'] <> ['A'..'Z'])

getScore target =
  fromMaybe 0 $ fst <$> find (\(score, item) -> item == target) scores

shared (a, b) = nub $ a `intersect` b

partOne = do
  parsed <- file

  let
    sharedItems = fmap (shared . split) parsed
    scored = concatMap (fmap getScore) sharedItems

  print (sum scored)

subGroups [] = []
subGroups (a:b:c:rest) = [[a, b, c]] <> subGroups rest

shared' [a, b, c] = nub $ (a `intersect` b) `intersect` c

partTwo = do
  parsed <- file

  let
    elfGroups = subGroups parsed
    badges = fmap shared' elfGroups
    scored = concatMap (fmap getScore) badges
    -- scored = concatMap (fmap getScore) sharedItems

  print (sum scored)
