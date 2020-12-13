module Main where

import System.Directory
import System.Environment
import Text.Parsec
import Flow
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec.Prim(Parser)
import Data.List.Split
import qualified Data.Set as Set
import Debug.Trace
import Data.List
import Data.Sort


part1 :: [Int] -> [Int] -> [Int]
part1 result (a1:a2:rest) = result ++ [abs (a2 - a1)] ++ part1 result (a2:rest)
part1 result [last] = result ++ [3]
part1 result [] = result ++ [3]


isBetween :: Int -> Int -> Int -> Bool
isBetween x a b =
  x >= a && x <= b

validConnection :: Int -> Int -> Bool
validConnection a b = isBetween (abs (b - a)) 1 3

type Connections = Map.Map Int [Int]

validConnections :: [Int] -> [Int]
validConnections (a:rest) = do
  rest |> takeWhile (\x -> validConnection a x)

buildGraph :: Connections -> [Int] -> Connections
buildGraph c [] = c
buildGraph connections l =
  let (h:rest) = l
      nextItems = validConnections l
      ret = Map.insert h nextItems connections
  in buildGraph ret rest

type Memo = Map.Map [Int] Int

countPathsInGraph :: Connections -> Memo -> [Int] -> Int -> (Int, Memo)
-- "key for memoization is the path up to `from`, and the value means number of paths that has this path that originate from this path"
countPathsInGraph graph memo path from =
   let targets = graph Map.! from :: [Int]
   in do
     target <- targets :: Int
     let newPath = path ++ [target]
     return (if (Map.member newPath memo)
        then (memo Map.! newPath, memo)
        else countPathsInGraph graph memo newPath target)

part2 list =
        let graph = buildGraph Map.empty list
            (count, memo) = trace ("Counting paths for graph: " ++ show graph) (countPathsInGraph graph Map.empty [] 0)
        in count


main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let input = [0] ++ (content |> splitOn "\n" |> filter (not . null) |> map read :: [Int])
  print input
  --let p1res = input |> sort |> part1 [] |> sort |> group |> map (\g -> ((g, length g)))

  let testInput = [0, 16 ,10 ,15 ,5 ,1 ,11 ,7 ,19 ,6 ,12 ,4] |> sort
  let testInput2 = [0, 28 ,33 ,18 ,42 ,31 ,14 ,46 ,20 ,48 ,47 ,24 ,23 ,49 ,45 ,19 ,38 ,39 ,11 ,1 ,32 ,25 ,35 ,8 ,17 ,7 ,9 ,4 ,2 ,34 ,10 ,3] |> sort
  --let testRes = testInput |> sort |> part1 [] |> sort |> group |> map (\g -> ((g, length g)))
  --print ("Part 1 result: " ++ show p1res)

  print ("Test 2 inp: " ++ show testInput2)
  let t1res = testInput |> sort |> part2
  print ("Part 2 test 1 result: " ++ show t1res)

  let t2res = testInput2 |> sort |> part2
  print ("Part 2 test 2 result: " ++ show t2res)

  let sortedInput = input |> sort
  print ("Input: " ++ show sortedInput)
  let p2res = sortedInput |> part2
  print ("Part 2 result: " ++ show p2res)
