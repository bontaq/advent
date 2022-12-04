module DayFour.Puzzle where

import Data.List

-----------
-- Model --
-----------

data Tile = Free Int | Called Int
  deriving (Show, Eq)

newtype Board = Board [[Tile]]
  deriving (Show, Eq)

-----------
-- Parse --
-----------

fileLocation = "./src/DayFour/data.txt"

boardChunks :: [String] -> [[String]]
boardChunks [] = []
boardChunks rows = takeWhile (/= "") rows : boardChunks (drop 1 (dropWhile (/= "") rows))

parseBoards rows =
  let
    boards = boardChunks rows
  in
    fmap (Board . fmap (fmap (Free . read :: String -> Tile) . words)) boards

parseCalled "" = []
parseCalled csv = (read (takeWhile (/= ',') csv) :: Int) : parseCalled (drop 1 (dropWhile (/= ',') csv))

parse = do
  (called:boards) <- lines <$> readFile fileLocation
  let boards' = parseBoards (drop 1 boards)
      called' = parseCalled called
  pure (called', boards')

--------------
-- Part One --
--------------

setCalled :: Int -> [Board] -> [Board]
setCalled number = fmap set
  where set (Board board) = Board $ fmap setInner board
        setInner row = fmap toggle row
        toggle (Free num) = if num == number then Called num else Free num
        toggle (Called num) = Called num

rotateBoard (row:rows)= foldr combine (fmap (: []) row) rows
  where combine toCombine acc = (\(a, b) -> b:a) <$> zip acc toCombine

checkWin = any (all isCalled)
  where
    isCalled (Called _) = True
    isCalled (Free _)   = False

findWin [] = []
findWin (Board board:boards) =
  if checkWin board || checkWin (rotateBoard board)
  then [board]
  else findWin boards

runCalls (call:nextCalls) boards =
  let newBoards = setCalled call boards
  in case findWin newBoards of
    [winner] -> (call, winner)
    [] -> runCalls nextCalls newBoards

sumUncalled board = sum $ collectUncalled board
  where collectUncalled xs = fmap toNumbers $ filter isUncalled $ mconcat xs
        isUncalled (Free _)   = True
        isUncalled (Called _) = False
        toNumbers (Free x) = x

partOne = do
  (toCall, boards) <- parse
  let (called, winningBoard) = runCalls toCall boards
  print (called * sumUncalled winningBoard)

--------------
-- Part two --
--------------

findWins boards = foldr (<>) [] $ fmap isWinner boards
  where isWinner (Board board) =
          [board | checkWin board || checkWin (rotateBoard board)]

runCalls' [] _ = []
runCalls' (call:nextCalls) boards =
  let newBoards = setCalled call boards
      winners = Board <$> findWins newBoards
  in (call, winners) : runCalls' nextCalls (newBoards \\ winners)

partTwo = do
  (toCall, boards) <- parse
  let wins = filter (\(_, xs) -> not (null xs)) $ runCalls' toCall boards
      (call, [Board lastWinner]) = last wins
      x = sumUncalled lastWinner
  print (call * x)
