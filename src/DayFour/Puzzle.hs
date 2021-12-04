module DayFour.Puzzle where

-----------
-- Model --
-----------

data Tile = Free Int | Called Int
  deriving Show

newtype Board = Board [[Tile]]
  deriving Show

-----------
-- Parse --
-----------

fileLocation = "./src/DayFour/testData.txt"

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
  pure boards'
