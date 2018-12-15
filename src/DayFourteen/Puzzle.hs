{-# LANGUAGE BangPatterns #-}

module DayFourteen.Puzzle where

import qualified Data.Vector.Unboxed as U
import Data.List (isInfixOf)

puzzleInput :: [Char]
puzzleInput = "793061"

type Board = U.Vector Char
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
  let score = toDigit $ board U.! pos
  in score + (getScore elfs board)

moveElfs :: [Elf] -> Board -> [Elf]
moveElfs [] _ = []
moveElfs ((Elf pos):elfs) board =
  let
    toMove      = (+1) $ toDigit $ board U.! pos
    newPosition = (pos + toMove) `mod` (U.length board)
  in Elf newPosition : moveElfs elfs board

-- test :: U.Vector Char -> (U.Vector Char, U.Vector Char)
-- test a = U.span (\b -> b U.elem target) a

loop :: Int -> [Elf] -> Board -> Board
loop 0 elfs board     = board
loop times elfs board =
  let score = getScore elfs board
      newBoard =
        foldr (\x a -> U.snoc a x)
        board
        -- no idea why I have to reverse this hm
        -- works normally w/o when using non-unboxed vector
        (reverse $ show score)
      newPosition = moveElfs elfs newBoard
  in loop (times - 1) newPosition newBoard

--
-- What are the scores of the ten recipes immediately
-- after the number of recipes in your puzzle input?
--
partOne = do
  print $ length . show $ puzzleInput
  let
    board = U.fromList ['3', '7'] :: Board
    initialElves = map Elf [0..(U.length board - 1)]
    ans = loop 10 initialElves board
    score = getScore initialElves board
    newBoard = foldr (\a b -> U.snoc b a) board (show score)
    newPosition = moveElfs initialElves newBoard
--  print $ initialElves
--  print $ newBoard
--  print newPosition

  -- lmao takes until the end of the universe

  print $ U.take 10 $ U.drop 793061 $ loop 800000 initialElves board
  pure ()

target :: [Char]
target = "793061"

loop' :: Bool -> [Elf] -> Board -> Board
loop' False elfs board = board
loop' times elfs board =
  let score = getScore elfs board
      newBoard =
        foldr (\x a -> U.snoc a x)
        board
        -- no idea why I have to reverse this hm
        -- works normally w/o when using non-unboxed vector
        (reverse $ show score)
      newPosition = moveElfs elfs newBoard

      -- hopefully more efficient?
      slicesPositions = map (\(Elf p) -> max 0 (p - 8)) elfs

      slices =
        map (\p ->
              U.slice p
              (min (U.length newBoard - p) (p + 8))
              -- (U.length newBoard - p)
              newBoard)
        slicesPositions
      valid = not $ any (\s -> isInfixOf target (U.toList s)) slices

      continue = valid

  in loop' continue newPosition newBoard

--
-- How many recipes appear on the scoreboard to
-- the left of the score sequence in your puzzle input?
--
partTwo = do
  let
    board = U.fromList ['3', '7'] :: Board
    initialElves = map Elf [0..(U.length board - 1)]
  print $ loop' True initialElves board
