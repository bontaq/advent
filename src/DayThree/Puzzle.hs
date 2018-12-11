{-# LANGUAGE QuasiQuotes #-}

module DayThree.Puzzle where

import qualified Data.Vector as V
import Data.List (intersect)
import           Text.Parser.Char (char, anyChar, newline, string)
import           Text.Parser.Combinators (manyTill, many)
import           Text.Parser.Token (token, integer)
import           Text.RawString.QQ (r)
import           Text.Trifecta.Parser (Parser, parseFromFile, parseString)

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

type PositionX = Int
type SizeX = Int
type Row = [Integer]

mkRow :: PositionX -> SizeX -> Row
mkRow start size = (replicate start 0) ++ (replicate size 1)

joinRows :: Row -> Row -> Row
joinRows a b
  | length a < length b = zipWith (+) (a ++ (repeat 0)) b
  | otherwise           = zipWith (+) a (b ++ (repeat 0))

handleClaims' :: [Claim] -> [Row] -> [Row]
handleClaims' [] oldRows = oldRows
handleClaims' (claim:claims) oldRows =
  let y            = (_y claim)
      height       = (_height claim)
      initialField = replicate (fromIntegral y) []
      field        = replicate (fromIntegral height) (mkRow
                                                      (fromIntegral $ _x claim)
                                                      (fromIntegral $ _width claim))
      newField     = zipWith joinRows (initialField ++ field ++ (replicate 1000 [])) oldRows
  in
    handleClaims' claims newField

handleClaims claims = handleClaims' claims (replicate 1000 [])

exampleInput = [r|#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
|]

partOne :: IO ()
partOne = do
  res <- parseFromFile (many row) "./src/DayThree/Data.txt"
  let field = fmap handleClaims res
      intersected = fmap (filter (> 1) . concat) field

  print $ fmap (length . head) field
  print $ fmap length field
  print $ fmap length intersected
  pure ()

removeId :: Integer -> [Claim] -> [Claim]
removeId target = filter (\claim -> (_id claim) /= target)

intersected :: Claim -> Claim -> Bool
intersected (Claim _ x y width height) (Claim _ x' y' width' height') =
  let intersectX = (> 0) . length $ intersect [x..(x + width - 1)] [x'..(x' + width' - 1)]
      intersectY = (> 0) . length $ intersect [y..(y + height - 1)] [y'..(y' + height' - 1)]
  in
    intersectX && intersectY

checkForTrue :: Claim -> [Bool] -> (Claim, Bool)
checkForTrue claim claims = (,) claim $ foldr (\b acc -> acc || b) False claims

handleOptions' :: [Integer] -> [Claim] -> [Claim] -> [(Claim, Bool)]
handleOptions' ids claims goodClaims =
  let all =
        map (\claim -> checkForTrue claim $
                       map
                           (\claim' ->
                             if (_id claim') /= (_id claim)
                             then intersected claim claim'
                             else False)
                           claims)
        claims
  in
    filter (\(a, b) -> b == False) all

handleOptions :: Maybe [Integer] -> Maybe [Claim] -> IO ()
handleOptions (Just ids) (Just claims) =
  print $ handleOptions' ids claims []

partTwo :: IO ()
partTwo = do
  res <- parseFromFile (many row) "./src/DayThree/Data.txt"
  let possibilities = fmap (map _id) res
  handleOptions possibilities res
  pure ()
