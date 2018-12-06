{-# LANGUAGE QuasiQuotes #-}

module DayThree.Puzzle  where

import qualified Data.Vector as V
import           Text.Parser.Char (char, anyChar, newline, string)
import           Text.Parser.Combinators (manyTill, many)
import           Text.Parser.Token (token, integer)
import           Text.RawString.QQ (r)
import           Text.Trifecta.Parser (Parser, parseFromFile)

exampleInput = [r|
#1 @ 861,330: 20x10
#2 @ 491,428: 28x23
#3 @ 64,746: 20x27
#4 @ 406,769: 25x28
|]


-- x and y start from the top left
-- width extends right
-- height extends down
data Claim = Claim { _id     :: Integer
                   , _x      :: Integer
                   , _y      :: Integer
                   , _width  :: Integer
                   , _height :: Integer
                   } deriving (Show)

row :: Parser Claim
row = do
  char '#'
  claim <- token integer
  string "@ "
  x <- token integer
  char ','
  y <- token integer
  char ':' <* char ' '
  width <- token integer
  char 'x'
  height <- token integer

  pure $ Claim claim x y width height


type Tapestry = [[Integer]]

claimToSet :: Claim -> Tapestry -> [Integer]
claimToSet (Claim _ x y width height) tap = undefined

findMaxX :: [Claim] -> Integer
findMaxX = maximum . map (\(Claim _ x _ _ height) -> x + height)

findMaxY :: [Claim] -> Integer
findMaxY = maximum . map (\(Claim _ _ y width _) -> y + width)

both :: (a -> c) -> (a -> d) -> a -> (c, d)
both g f a = (g a, f a)

-- 3,2 5x4
handleClaims :: [Claim] -> [[Integer]]
handleClaims claims = do
  let (sizeX, sizeY) = both findMaxX findMaxY claims
  [[sizeX, sizeY]]

partOne :: IO ()
partOne = do
  res <- parseFromFile (many row) "./src/DayThree/Data.txt"
  print $ fmap handleClaims res
  pure ()
