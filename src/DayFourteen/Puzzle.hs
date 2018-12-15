module DayFourteen.Puzzle where

import qualified Data.Vector.Unboxed as U
import Data.List (isInfixOf)
import Data.Sequence
import qualified Data.Sequence as Seq

puzzleInput :: [Char]
puzzleInput = "793061"

type Board = Seq Char
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
  let score = toDigit $ index board pos
  in score + (getScore elfs board)

moveElfs :: [Elf] -> Board -> [Elf]
moveElfs [] _ = []
moveElfs ((Elf pos):elfs) board =
  let
    toMove      = (+1) $ toDigit $ index board pos
    newPosition = (pos + toMove) `mod` (Seq.length board)
  in Elf newPosition : moveElfs elfs board

-- test :: U.Vector Char -> (U.Vector Char, U.Vector Char)
-- test a = U.span (\b -> b U.elem target) a

loop :: Int -> [Elf] -> Board -> Board
loop 0 elfs board     = board
loop times elfs board =
  let score = getScore elfs board
      newBoard = board <> (fromList $ show score)
      -- newBoard =
      --   foldr (\x a -> U.snoc a x)
      --   board
      --   -- no idea why I have to reverse this hm
      --   -- works normally w/o when using non-unboxed vector
      --   (reverse $ show score)
      newPosition = moveElfs elfs newBoard
  in loop (times - 1) newPosition newBoard

--
-- What are the scores of the ten recipes immediately
-- after the number of recipes in your puzzle input?
--
partOne = do
  -- print $ length . show $ puzzleInput
  let
    board = fromList ['3', '7'] :: Board
    initialElves = map Elf [0..(Seq.length board - 1)]
    ans = loop 10 initialElves board
    score = getScore initialElves board
    -- newBoard = foldr (\a b -> U.snoc b a) board (show score)
    -- newPosition = moveElfs initialElves newBoard
--  print $ initialElves
--  print $ newBoard
--  print newPosition

  -- lmao takes until the end of the universe

  print $ Seq.take 10 $ Seq.drop 793061 $ loop 800000 initialElves board
  pure ()

target :: [Char]
target = "793061"

loop' :: [Elf] -> Board -> [Char]
loop' elfs board =
  let score = getScore elfs board
      newDigits = fromList $ show score
      newBoard = board <> (newDigits)
      newPosition = moveElfs elfs newBoard
  in (show score) ++ loop' newPosition newBoard

--
-- How many recipes appear on the scoreboard to
-- the left of the score sequence in your puzzle input?
--
partTwo = do
  let
    board = fromList ['3', '7'] :: Board
    initialElves = map Elf [0..(Seq.length board - 1)]
  print $ Prelude.take 10 $ Prelude.drop 793061 $ '3' : '7' : loop' initialElves board
