{-# LANGUAGE BangPatterns #-}

module DayFourteen.Puzzle where

puzzleInput :: [Char]
puzzleInput = "793061"

type Board = [Char]
type CurrentPosition = Int

data Elf = Elf CurrentPosition
         deriving Show

toDigit :: Char -> Int
toDigit = (\x -> read (x : "") :: Int)

-- 1) pick scores from positions
-- 2) sum
-- 3) append sum to puzzleInput
-- 4) move all current score + 1

getScore :: [Elf] -> Board -> Int
getScore [] _ = 0
getScore ((Elf pos):elfs) board =
  let score = toDigit $ board !! pos
  in score + (getScore elfs board)

moveElfs :: [Elf] -> Board -> [Elf]
moveElfs [] _ = []
moveElfs ((Elf pos):elfs) board =
  let toMove      = (+1) $ toDigit $ board !! pos
      newPosition = (pos + toMove) `mod` (length board)
  in Elf newPosition : moveElfs elfs board

loop :: Int -> [Elf] -> Board -> Board
loop 0 elfs board     = board
loop times elfs board =
  let !score = getScore elfs board
      !newBoard = board ++ (show score)
      !newPosition = moveElfs elfs newBoard
  in loop (times - 1) newPosition newBoard

--
-- What are the scores of the ten recipes immediately
-- after the number of recipes in your puzzle input?
--
partOne = do
  print $ length . show $ puzzleInput
  let
    board = "37"
    initialElves = map Elf [0..(length board - 1)]
    score = getScore initialElves board
    newBoard = board ++ (show score)
    newPosition = moveElfs initialElves newBoard
  print $ initialElves
  print $ newBoard
  print newPosition

  -- lmao takes until the end of the universe
  print $ take 10 $ drop 2018 $ loop 1000 initialElves board
