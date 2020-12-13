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

step :: Char -> [Char] -> Char
step seat neighbors =
  case seat of
      'L' -> if '#' `notElem` neighbors
                then '#'
                else 'L'
      '#' -> if (neighbors |> filter ('#'==) |> length) >= 4
                then 'L'
                else '#'
      '.' -> '.'



gatherNeighbors :: [[Char]] -> Int -> Int -> Int -> Int -> [Char]
gatherNeighbors seating w h x y =
  let area = [(i + x, j + y) | i <- [-1..1], j <- [-1..1]] |> filter (\(x,y) -> x >= 0 && x < w && y >= 0 && y < h)
  in trace ("Area: " ++ show area) area |> map (\(i,j) -> trace ("X: " ++ show i ++ " y: " ++ show j) seating !! j !! i)

part1 input =
  let h = trace ("Height: " ++ show (length input)) length input
      w = trace ("Width: " ++ show (length (head input))) (length (head input))
  in [(x,y) | x <- [0..w], y <- [0..h]] |> map (\(x,y) -> let ns = gatherNeighbors input w h x y in step (input !! y !! x) (trace ("NS: " ++ show ns) ns))


main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let input = content |> splitOn "\n" |> filter (not . null)
  let resP1 = part1 input
  print ("Part 1 result: " ++ show resP1)
  --print content
