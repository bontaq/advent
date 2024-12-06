{-# LANGUAGE QuasiQuotes #-}
module DayFive.Puzzle where

import Text.RawString.QQ
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Trifecta.Parser
import Control.Applicative
import Text.Trifecta (Result)
import Debug.Trace (trace)

location = "./src/DayFive/data.txt"

test = [r|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|]

data Rule = Rule
  { earlierPage :: Integer
  , page :: Integer
  } | Update [Integer] | None deriving Show

parseRule :: Parser Rule
parseRule = do
  earlierPage <- integer
  char '|'
  page <- integer
  pure $ Rule earlierPage page

parseUpdateNumber = do
  num <- integer
  char ','
  pure num

parseUpdate :: Parser Rule
parseUpdate = do
  numbers <- some (try parseUpdateNumber)
  lastNumber <- integer
  pure $ Update (numbers <> [lastNumber])

parseNewline = do
  newline
  pure $ None

parseIt = try parseUpdate <|> try parseRule <|> parseNewline

-- I suppose we only care about things before the rule,
-- since that invalidates it
-- reverse
-- get number
-- filter by page
-- is the earlierPage in? success
-- 75, filter page by, is the earlierPage in? failed

-- or we can do
-- get number
-- filter by page
-- is priorPage in the rest of the list? failed

isRule target (Rule { page }) = target == page
isRule target _               = False

getRules page = filter (isRule page)

isUpdate (Update _) = True
isUpdate _          = False

getUpdates = filter isUpdate

getPriorPages :: [Rule] -> [Integer]
getPriorPages = fmap getPriorPage
  where
    getPriorPage (Rule { earlierPage }) = earlierPage

checkUpdate' :: [Rule] -> [Integer] -> Bool
checkUpdate' _ [] = False
checkUpdate' rules (page:pages) =
  let
    toCheck = getRules page rules
    priorPages = getPriorPages toCheck
    brokeRules = fmap (`elem` pages) priorPages
  in
    any (== True) brokeRules || checkUpdate' rules pages

checkUpdate rules (Update updates) = checkUpdate' rules updates

getMiddle (Update updates) = updates !! (length updates `div` 2)

runIt raw = do
  parsed <- parseString (some parseIt) mempty raw

  let
    updates = getUpdates parsed
    checked = fmap (\update -> (checkUpdate parsed update, update)) updates
    valid = filter (\(failed, _) -> not failed) checked
    middles = fmap (\(_, updates) -> getMiddle updates) valid

  pure (sum middles, middles)

partOneEx = do
  print $ runIt test

partOne = do
  raw <- readFile location
  print $ runIt raw

insertAt :: Int -> a -> [a] -> [a]
insertAt place item items =
  let
    (a,b) = splitAt place items
  in
    a <> [item] <> b

removeAt :: Int -> [a] -> [a]
removeAt place items =
  let (a,b) = splitAt place items
  in a <> drop 1 b

fix :: [Rule] -> [Integer] -> [Integer]
fix rules [] = []
fix rules (item:items) =
  let
    applicableRules = getRules item rules
    priorPages = getPriorPages applicableRules
    toMoveAhead = filter (`elem` items) priorPages
    remaining = filter (`notElem` toMoveAhead) items
  in
    toMoveAhead <> [item] <> fix rules remaining

refix :: [Rule] -> [Integer] -> [Integer]
refix rules items =
  let fixed = fix rules items
  in
    if not $ checkUpdate' rules fixed
    then fixed
    else refix rules fixed

-- runIt' :: String -> Result (Integer, [Integer])
runIt' raw = do
  parsed <- parseString (some parseIt) mempty raw

  let
    updates = getUpdates parsed
    checked = fmap (\update -> (checkUpdate parsed update, update)) updates
    -- for this one we want the failures
    invalid = fmap (\(_, Update items) -> items) $ filter (\(failed, _) -> failed) checked
    fixed = fmap (refix parsed) invalid
    middles = fmap (\fix -> fix !! (length fix `div` 2)) fixed

  --pure middles
  pure (sum middles, middles)

partTwoEx = do
  print $ runIt' test

partTwo = do
  raw <- readFile location
  print $ runIt' raw
