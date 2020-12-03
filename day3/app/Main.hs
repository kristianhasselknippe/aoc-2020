module Main where

import System.Directory
import Data.List.Split
import System.Environment(getArgs)

parseInput :: String -> IO String
parseInput path = do
  currentDir <- getCurrentDirectory
  readFile (currentDir ++ "/" ++ path)

hasTree :: String -> Int -> Bool
hasTree line xPos =
  let lineLen = length line
      index = xPos `mod` lineLen
  in (line !! index) == '#'

countTreesInPath :: [String] -> (Int, Int) -> Int
countTreesInPath lines (right, down) =
  let linesInPath = map snd $ filter (\(i,_) -> i `mod` down == 0) $ zip [0..] lines
  in sum $ map (\x -> if x then 1 else 0) $ zipWith hasTree linesInPath [x * right | x <- [0..]]

main :: IO ()
main = do
  argv <- getArgs
  print argv
  input <- parseInput (head argv)
  print input
  let lines = filter (not . null)  $ splitOn "\n" input
  let resultP1 = countTreesInPath lines (3, 1)
  print ("Result p1: " ++ show resultP1)
  let paths = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let resultP2 = product $ map (countTreesInPath lines) paths
  print ("result p2: " ++ show resultP2)
