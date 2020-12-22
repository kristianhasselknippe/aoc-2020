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
import Data.Array
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


neighborhood x y w h =
  [(i + x, j + y) | i <- [-1..1], j <- [-1..1]]
      |> filter (\(x,y) -> x >= 0 && x < w && y >= 0 && y < h) |> delete (x,y)


gatherNeighbors :: Array Int Char -> (Int, Int) -> (Int, Int) -> [Char]
gatherNeighbors seating (w, h) (x, y) =
  let n = neighborhood x y w h
  in n |> map (\(x,y) -> seating ! (w * y + x))

part1Update :: (Int, Int) -> Array Int Char -> Array Int Char
part1Update (w, h) input =
   [(x,y) | y <- [0..h-1], x <- [0..w-1]]
        |> map (\(x,y) -> let ns = gatherNeighbors input (w, h) (x, y)
                          --in step (input ! (trace ("WH: " ++ show (w,h) ++ ", XY: " ++ show (x,y) ++ ", I: " ++ show (y * w + x)) (y * w + x))) ns
                          in step (input ! (y * w + x)) ns
      )
        |> listArray (0, w * h - 1)

simulate :: (Int, Int) -> Array Int Char -> Array Int Char
simulate size input =
   let newBoard = part1Update size input
   in if (trace ("\nBO: " ++ show (newBoard |> elems)) newBoard) /= input
   --in if newBoard /= input
     then simulate size newBoard
     else newBoard

countOccupied :: Array Int Char -> Int
countOccupied input =
    input |> elems |> filter ('#' ==) |> length

part1 input =
   let h = length input
       w = length (head input)
       arrayInput = (trace ("W: " ++ show w ++ ", H: " ++ show h ++ " Len: " ++ show (w * h)) listArray) (0, w * h - 1) (input |> concat)
       res = simulate (w, h) arrayInput
   in countOccupied res


main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let input = content |> splitOn "\n" |> filter (not . null) |> map (filter (/='\r'))
  print ("Input: " ++ show input)
  let resP1 = part1 input
  print ("Part 1 result: " ++ show resP1)
  --print content
