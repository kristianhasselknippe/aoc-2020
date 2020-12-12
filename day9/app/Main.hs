{-# LANGUAGE TupleSections #-}
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

isValid :: Int -> [Int] -> Bool
isValid current prev =
  trace ("\nTest " ++ show current ++ " with " ++ show prev) [(x,y) | (x:xs) <- tails prev, y <- xs] |> any (\(x, y) -> trace ("Testin: " ++ show x ++ ", " ++ show y) (x + y == current))

prevN :: Int -> Int -> [Int] -> [Int]
prevN n at items =
   items |> drop (at - n) |> take n

-- |> sort

part1 :: [Int] -> Int -> [(Bool, Int)]
part1 input window =
   let dropFirst = window + 1
   in zip (drop window input) [window..] |> map (\(x, i) -> trace ("We are at: " ++ show i) (prevN window i input |> isValid x |> (,x)))


dropUntilLessThan x list
   | sum list > x = dropUntilLessThan x (tail list)
   | sum list <= x = list


part2 :: Int -> [Int] -> [(Int, Int)] -> [Int]
part2 toFind accum ((x,i):rest) =
  if x >= toFind
     then part2 toFind accum rest
     else
        let newAccum = dropUntilLessThan toFind (accum ++ [x])
        in if length newAccum > 1 && toFind == sum newAccum
          then newAccum
          else part2 toFind newAccum rest
part2 toFind accum [] =
  accum

main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let test = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
  let res = part1 test 5
  print ("Result test: " ++ show res)
  let inputData = splitOn "\n" content |> filter (not . null) |> map read :: [Int]
  print ("Input data: " ++ show inputData)
  let part1Res = part1 inputData 25 |> filter (not . fst)
  print ("Part 1 result: " ++ show part1Res)

  let [(_,p1)] = part1Res

  let part2Range = part2 p1 [] (zip inputData [0..])
  print ("Part2 range: " ++ show part2Range ++ " which has sum: " ++ show (sum part2Range))
  let sorted = part2Range |> sort
  let resPart2 = head sorted + last sorted
  print ("Part2 result: " ++ show resPart2)
