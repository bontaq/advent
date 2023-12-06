{-# LANGUAGE QuasiQuotes #-}

module DayThree.Puzzle where

import Text.RawString.QQ
import Text.Read (readMaybe)
import Data.Foldable (find)
import Data.Maybe (isJust, catMaybes, mapMaybe)
import Data.List (nub)

test = [r|
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|]

data Symbol
  = Number [(Integer, Integer)] Integer
  | Symbol (Integer, Integer) String
  deriving (Show, Eq)

-- ..12..13
isDigit (_, c) = case readMaybe [c] :: Maybe Integer of
  Just number -> True
  Nothing     -> False

findNumbers [] = []
findNumbers xs =
  let
    number = takeWhile isDigit xs
    remain = if not (null number) then dropWhile isDigit xs else drop 1 xs
  in
    if not (null number)
    then [number] <> findNumbers remain
    else findNumbers remain

isSymbol c = not (isDigit c) && (snd c /= '.')

findSymbols [] = []
findSymbols xs = filter isSymbol xs

combineNumber :: Integer -> [(Integer, Char)] -> Symbol
combineNumber row nums =
  let
    digits = fmap snd nums
    coors = fmap ((row,) . fst) nums
  in
    Number coors (read digits :: Integer)

combineSymbols :: Integer -> (Integer, Char) -> Symbol
combineSymbols row (x, char) =
  Symbol (row, x) [char]

-- lexRow :: Row -> String -> [Symbol]
lexRow (row, items) =
  let
    locations = zip [0..] items
    numbers = combineNumber row <$> findNumbers locations
    symbols = combineSymbols row <$> findSymbols locations
  in
    numbers <> symbols

isSymbol' (Symbol _ _) = True
isSymbol' _            = False

isNumber' (Number _ _) = True
isNumber' _            = False

shiftPoint num (y, x) = (y, x + num)

expandPoints :: [(Integer, Integer)] -> [(Integer, Integer)]
expandPoints points =
  let
    newPoints =
      [shiftPoint (-1) $ head points]
      <> [shiftPoint 1 $ last points]
      <> points
    upperRow = (\(y, x) -> (y - 1, x)) <$> newPoints
    lowerRow = (\(y, x) -> (y + 1, x)) <$> newPoints
  in
    upperRow <> lowerRow <> newPoints

testNumber :: Symbol -> [(Integer, Integer)] -> Bool
testNumber (Number points val) symbols =
  let
    expanded = expandPoints points
    tested = fmap
      (\point -> find (point ==) symbols)
      expanded
  in
    any isJust tested

solveOne rows =
  let
    indexed = zip [0..] rows
    lexed = concatMap lexRow indexed
    symbols = filter isSymbol' lexed
    symbolPoints = (\(Symbol point _) -> point) <$> symbols
    numbers = filter isNumber' lexed
  in
    filter
      (`testNumber` symbolPoints)
      numbers

sumSym xs = sum $ fmap (\(Number _ num) -> num) xs

location = "./src/2023/DayThree/data.txt"

partOne = do
  raw <- readFile location

  let
    input = lines raw
    ans = solveOne input

  mapM_ print ans
  print (sumSym ans)

isGear (Symbol _ "*") = True
isGear _              = False

getGear :: Symbol -> [Symbol] -> Maybe Integer
getGear (Symbol (y, x) _) numbers =
  let
    expanded = expandPoints [(y, x)]
    search = nub $ mapMaybe
      (\point ->
         find (\(Number points _) -> point `elem` points) numbers
      )
      expanded
  in case search of
    [Number _ val, Number _ val'] -> Just $ val * val'
    _                             -> Nothing

solveTwo rows =
  let
    indexed = zip [0..] rows
    lexed = concatMap lexRow indexed
    possibleGears = filter isGear lexed
    numbers = filter isNumber' lexed
  in
    fmap (\gear -> getGear gear numbers) possibleGears

partTwo = do
  raw <- readFile location

  let
    input = lines raw
    ans = solveTwo input

  mapM_ print ans

  let
    final = sum $ catMaybes ans

  print final

-- so i could just line them up with each other
-- or i could do it manually
-- i could annotate it, but that doesn't help finding
-- the numbers
-- i think you can try something a little more challenging?
