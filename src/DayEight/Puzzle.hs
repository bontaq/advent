module DayEight.Puzzle where


-- 25 pixels wide and 6 pixels tall.

type Width = Int
type Height = Int

infini f x = f x : infini f (f x)

layerize :: Width -> Height -> [a] -> [[a]]
layerize x y p =
  let grouped = take y $ infini (take x) p
  in grouped

testInput = "123456789012"

parse :: String -> [Int]
parse = fmap (\x -> read (x : "") :: Int)

testLayerize = layerize 3 2 (parse testInput)
