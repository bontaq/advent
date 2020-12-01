module DayOne.Puzzle where

import Text.Parser.Combinators (many)
import Text.Parser.Token (token, integer)
import Text.Trifecta.Parser (Parser, parseFromFile)

import Data.Maybe (fromJust)
import Data.Holmes

numbers :: Parser [Integer]
numbers = many (token integer)

-- The following cool way didn't work

-- constraintSolve :: [Raw (Defined Int)] -> IO (Maybe [ Intersect Int ])
-- constraintSolve nums = do
--   let guesses = 2 `from` nums

--   guesses `satisfying` \[a, b] -> and'
--     [ distinct [a, b]
--     , (a .+ b) .== 2020 ]

-- partOne :: IO ()
-- partOne = do
--   nums <- fromJust <$> parseFromFile numbers "./src/DayOne/data.txt"

--   constraintSolve (fmap fromInteger nums) >>= print

partOne :: IO ()
partOne = do
  nums <- fromJust <$> parseFromFile numbers "./src/DayOne/data.txt"
  print [a * b | a <- nums
               , b <- nums
               , a + b == 2020 ]

partTwo :: IO ()
partTwo = do
  nums <- fromJust <$> parseFromFile numbers "./src/DayOne/data.txt"
  print [a * b * c | a <- nums
                   , b <- nums
                   , c <- nums
                   , a + b + c == 2020 ]
