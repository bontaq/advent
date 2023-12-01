{-# LANGUAGE NamedFieldPuns #-}
-- |

module DayEleven.Puzzle where

import Data.List
import Data.List.Split
import Control.Lens
import Debug.Trace

location = "./src/DayEleven/test.txt"

data Monkey = Monkey
  { name :: Int
  , items :: [Integer]
  , operation :: [String]
  , divisible :: Int
  , ifTrue :: Int
  , ifFalse :: Int
  } deriving (Show, Eq)

parseName raw =
  let [_, name] = words raw
  in (read (init name) :: Int)

parseItems :: String -> [Integer]
parseItems raw =
  let items = drop 2 $ words $ filter (/= ',') raw
  in fmap read items

parseOperation raw =
  let operation = drop 1 $ words raw
  in operation

parseTest raw =
  let rawDivisible = (words raw !! 3)
  in read rawDivisible :: Int

parseCond raw =
  let toMonkey = (words raw !! 5)
  in read toMonkey :: Int

parseMonkey raw =
  let
    [name, items, operation, test, cond1, cond2] = lines raw
    name' = parseName name
    items' = parseItems items
    operation' = parseOperation operation
    test' = parseTest test
    ifTrue = parseCond cond1
    ifFalse = parseCond cond2
  in Monkey name' items' operation' test' ifTrue ifFalse

parse = do
  raw <- readFile location

  let rawMonkeys = splitOn "\n\n" raw
      monkeys = fmap parseMonkey rawMonkeys

  pure monkeys

type TargetMonkey = Int
type Stress = Integer

runOperation :: [String] -> Integer -> Integer
runOperation ["new", "=", a, op, b] amt =
  let
    rewrite x = if x == "old" then amt else (read x :: Integer)
    a' = rewrite a
    b' = rewrite b
  in case op of
    "+" -> (a' `mod` 9699690) + (b' `mod` 9699690)
    "*" -> (a' `mod` 9699690) * (b' `mod` 9699690)

runCond :: Int -> Int -> Int -> Integer -> Int
runCond divisibleBy ifTrue ifFalse amt =
  if amt `mod` (toInteger divisibleBy) == 0 then ifTrue else ifFalse

turn :: Monkey -> [(TargetMonkey, Stress)]
turn Monkey { items, operation, divisible, ifTrue, ifFalse } =
  let
    worryLevels = fmap (runOperation operation) items
    -- afterBored = fmap (`div` 3) worryLevels
    -- change for part 2, no longer divided by 3
    afterBored = worryLevels
    tested = fmap (runCond divisible ifTrue ifFalse) afterBored
  in zip tested afterBored

applyChange :: [Monkey] -> (TargetMonkey, Stress) -> [Monkey]
applyChange monkeys (target, stress) =
  over (element target) (\m@Monkey {items} -> m { items=items <> [stress] }) monkeys

applyChanges :: [Monkey] -> [(TargetMonkey, Stress)] -> [Monkey]
applyChanges = foldl applyChange

runRound :: Int -> [Monkey] -> [Int] -> IO ([Int], [Monkey])
runRound target monkies counts =
  if target < length monkies
  then
    let
      monkey = monkies !! target
      -- 1. get changes
      changes = turn monkey
      -- 2. update count
      itemCount = length $ items monkey
      counts' = over (element target) (+ itemCount) counts
      -- 2. clear items from monkey as they're thrown
      monkies' = over (element target) (\m -> m { items=[] }) monkies
      -- 3. apply changes
      finalMonkies = applyChanges monkies' changes
    in do
      runRound (target + 1) finalMonkies counts'
  else
    pure (counts, monkies)

runRounds 0 monkeys counts = pure (monkeys, counts)
runRounds n monkeys counts = do
  (counts', monkeys') <- runRound 0 monkeys counts
  runRounds (n - 1) monkeys' counts'

partOne = do
  monkeys <- parse

--  (counts, monkeys) <- runRound 0 monkeys (replicate (length monkeys) 0)
--
--  mapM_ print monkeys
--  print counts

  (monkeys', counts') <- runRounds 10000 monkeys (replicate (length monkeys) 0)

  mapM_ print monkeys'
  print (sort counts')
