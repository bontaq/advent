module DaySeven.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (sortBy)
import Control.Lens
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Debug.Trace

number :: Parser Int
number = do
  i <- integer
  _ <- optional (char ',')
  pure (fromIntegral i)

numbers :: Parser [Int]
numbers = many number

data Mode = I | P
            deriving (Show, Eq)

mode '1' = I
mode '0' = P

parseOp :: Int -> (Int, [Mode])
parseOp c =
  let s = show c
  in case length s of
    1 -> (read s :: Int, [P, P])
    2 -> (read s :: Int, [P, P])
    3 ->
        let
          stupid = "0" <> (take (length s - 1) s)
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          z = drop ((length s) - 1) s
        in
          ((read z :: Int), [mode a, mode b])
    4 ->
        let
          stupid = take (length s - 2) s
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          z = drop ((length s) - 2) s
        in
          ((read z :: Int), [mode a, mode b])
    _ -> error s


type Done = Bool
type Offset = Int
type ProgramOut = (Done, Offset, Program, IO')

runProgram :: Int -> Vector Int -> ([Int], [Int]) -> ProgramOut
runProgram offset program io | trace (show  " " <> show io) False = undefined
runProgram offset program io =
  let
    opcode = program ! offset
    (op, modes) = parseOp opcode

    store = program ! (offset + 3)
  in
    case op of
      99 -> (True, offset, program, io)
      _  -> run
        where run = case op of
                -- addition
                1 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (+) v0 v1)]) io

                -- multiply
                2 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (*) v0 v1)]) io

                -- store input
                3 ->
                  let (ins, out) = io
                  in case length ins of
                    0 -> (False, offset, program, io) -- return it for continuing to run
                    _ -> let (i:is) = ins
                         in
                           runProgram (offset + 2) (program // [(program ! (offset + 1), i)]) (is, out)

                -- output
                4 ->
                  let (i, o) = io
                      (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                  in runProgram (offset + 2) program (i, v0 : o)

                -- jump if true
                5 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 /= 0
                    then runProgram v1 program io
                    else runProgram (offset + 3) program io

                -- jump if false
                6 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == 0
                    then runProgram v1 program io
                    else runProgram (offset + 3) program io

                -- less than
                7 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 < v1
                    then runProgram (offset + 4) (program // [(store, 1)]) io
                    else runProgram (offset + 4) (program // [(store, 0)]) io

                -- equals
                8 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == v1
                    then runProgram (offset + 4) (program // [(store, 1)]) io
                    else runProgram (offset + 4) (program // [(store, 0)]) io

                e -> error $ show offset <> " " -- <> show program

type IO' = ([Int], [Int])
type Program = Vector Int
type ThrusterOut = [Int]

runVariation :: Program -> (Int, Int, Int, Int, Int) -> ([Int], ThrusterOut)
runVariation program (a,b,c,d,e) =
  let
    (_, _, _, (_, runAOut)) = runProgram 0 program ([a, 0], [])
    (_, _, _, (_, runBOut)) = runProgram 0 program ([b, head runAOut], [])
    (_, _, _, (_, runCOut)) = runProgram 0 program ([c, head runBOut], [])
    (_, _, _, (_, runDOut)) = runProgram 0 program ([d, head runCOut], [])
    (_, _, _, (_, runEOut)) = runProgram 0 program ([e, head runDOut], [])
  in
    ([a,b,c,d,e], runEOut)

filterUniq :: [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)]
filterUniq = filter (not . dupe)
  where
    dupe (a,b,c,d,e) =
       let
         n x = filter (\y -> y == x) [a,b,c,d,e]
       in
         any (\x -> x > 1) $ fmap (length . n) [a,b,c,d,e]

runVariations :: Program -> [([Int], ThrusterOut)]
runVariations program =
  let
    variations = [(a,b,c,d,e) | a <- [0..4], b <- [0..4], c <- [0..4], d <- [0..4], e <- [0..4]]
    filtered = filterUniq variations
  in
    fmap (runVariation program) filtered

partOne = do
  f <- readFile "./src/DaySeven/data.txt"
  let
    program = parseString numbers mempty f
    variations = fmap (runVariations . fromList) program

  print $ fmap (sortBy (\(_, a) (_, b) -> compare b a)) variations

runComputer :: TChan Int -> TChan Int -> Int -> ProgramOut -> IO ()
runComputer hout hin phase (done, offset, program, io) = do
  let (input', output') = io

  realInput <- if length input' == 0 && done == False
               then atomically $ fmap (\x -> [x]) $ readTChan hin
               else if length input' == 0
                       then pure []
                       else pure input'

  let newProgram@(done', offset', program', (i, o)) = runProgram offset program (realInput, [])

  case length o of
    0 -> do -- runComputer hout hin phase newProgram
      if not done'
      then runComputer hout hin phase newProgram
      else print newProgram
          --atomically $ writeTChan hout (head o)
    1 -> do
      atomically $ writeTChan hout (head o)
      runComputer hout hin phase newProgram
    _ -> error "fuck"

  -- runComputer hout hin phase newProgram


-- getStartSignal ((_, _, _, (_, s)):_) = s

-- continueVariation :: Program -> [ProgramOut] -> (Int, Int, Int, Int, Int) -> (Bool, [Int], ThrusterOut)
-- continueVariation program lastRun (a,b,c,d,e) =
--   let
--     startSignal = getStartSignal lastRun
--     offsets = fmap (\(_, offset, _, _) -> offset) lastRun
--     programs = fmap (\(_, _, program, _) -> program) lastRun
--     dones = fmap (\(done, _, _, _) -> done) lastRun

--     pA@(d1, _, _, (_, runAOut)) = runProgram (offsets !! 0) (programs !! 0) (startSignal, [])
--     pB@(d2, _, _, (_, runBOut)) = runProgram (offsets !! 1) (programs !! 1) (runAOut, [])
--     pC@(d3, _, _, (_, runCOut)) = runProgram (offsets !! 2) (programs !! 2) (runBOut, [])
--     pD@(d4, _, _, (_, runDOut)) = runProgram (offsets !! 3) (programs !! 3) (runCOut, [])
--     pE@(d5, _, _, (_, runEOut)) = runProgram (offsets !! 4) (programs !! 4) (runDOut, [])

--     done = all (==True) [d1, d2, d3, d4, d5]
--     newComputers = [pA, pB, pC, pD, pE]
--   in
--     if done
--     then (done, [a,b,c,d,e], runEOut)
--     else continueVariation program [pA, pB, pC, pD, pE] (a,b,c,d,e)

-- runVariation' :: Program -> Maybe [ProgramOut] -> (Int, Int, Int, Int, Int) -> (Bool, [Int], ThrusterOut)
-- runVariation' program lastRun (a,b,c,d,e) =
--   let
--     startSignal = 0
--     pA@(d1, _, _, (_, runAOut)) = runProgram 0 program ([a, startSignal], [])
--     pB@(d2, _, _, (_, runBOut)) = runProgram 0 program ([b, head runAOut], [])
--     pC@(d3, _, _, (_, runCOut)) = runProgram 0 program ([c, head runBOut], [])
--     pD@(d4, _, _, (_, runDOut)) = runProgram 0 program ([d, head runCOut], [])
--     pE@(d5, _, _, (_, runEOut)) = runProgram 0 program ([e, head runDOut], [])

--     done = d5
--   in
--     if done
--     then (done, [a,b,c,d,e], runEOut)
--     else continueVariation program [pA, pB, pC, pD, pE] (a,b,c,d,e)

-- runVariations' :: Program -> [(Bool, [Int], ThrusterOut)]
runVariations' :: Program -> IO ()
runVariations' program = do
  -- let
  a <- atomically newTChan
  b <- atomically newTChan
  c <- atomically newTChan
  d <- atomically newTChan
  e <- atomically newTChan
    -- variations = [(a,b,c,d,e) | a <- [5..9], b <- [5..9], c <- [5..9], d <- [5..9], e <- [5..9]]
    -- filtered = filterUniq variations
    -- filtered = [(9,8,7,6,5)]

  forkIO $ runComputer b a 0 (False, 0, program, ([9, 0], []))
  forkIO $ runComputer c b 0 (False, 0, program, ([7], []))
  forkIO $ runComputer d c 0 (False, 0, program, ([8], []))
  forkIO $ runComputer e d 0 (False, 0, program, ([5], []))
  forkIO $ runComputer a e 0 (False, 0, program, ([6], []))

  -- atomically $ writeTChan a 9
  -- atomically $ writeTChan b 7
  -- atomically $ writeTChan c 8
  -- atomically $ writeTChan d 5
  -- atomically $ writeTChan e 6

  -- atomically $ writeTChan b 0

  threadDelay(1000000000)

  pure ()
  -- in
  --   fmap (runVariation' program Nothing) filtered

fromResult (Success a) = a

partTwo = do
  f <- readFile "./src/DaySeven/smallData.txt"
  let
    program = fromResult $ parseString numbers mempty f

  variations <- (runVariations' . fromList) program

  -- v <- variations

  -- print $ fmap (sortBy (\(_, _, a) (_, _, b) -> compare b a)) variations

  pure ()
