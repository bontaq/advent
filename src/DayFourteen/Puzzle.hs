{-# LANGUAGE BangPatterns #-}

module DayFourteen.Puzzle where

import qualified Data.Vector as V

puzzleInput :: [Char]
puzzleInput = "793061"

type Board = V.Vector Char
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
  let score = toDigit $ board V.! pos
  in score + (getScore elfs board)

moveElfs :: [Elf] -> Board -> [Elf]
moveElfs [] _ = []
moveElfs ((Elf pos):elfs) board =
  let
    toMove      = (+1) $ toDigit $ board V.! pos
    newPosition = (pos + toMove) `mod` (length board)
  in Elf newPosition : moveElfs elfs board

loop :: Int -> [Elf] -> Board -> Board
loop 0 elfs board     = board
loop times elfs board =
  let !score = getScore elfs board
      !newBoard = foldr (\x a -> V.snoc a x) board (show score)
      !newPosition = moveElfs elfs newBoard
  in loop (times - 1) newPosition newBoard

--
-- What are the scores of the ten recipes immediately
-- after the number of recipes in your puzzle input?
--
partOne = do
  print $ length . show $ puzzleInput
  let
    board = V.fromList ['3', '7'] :: Board
    initialElves = map Elf [0..(length board - 1)]
    ans = loop 10 initialElves board
--    score = getScore initialElves board
--    newBoard = board ++ (show score)
--    newPosition = moveElfs initialElves newBoard
  print $ initialElves
--  print $ newBoard
--  print newPosition

  -- lmao takes until the end of the universe

  print $ V.take 10 $ V.drop 793061 $ loop 1000000 initialElves board
  pure ()
