module Main where

--import Text.Parsec
import System.Directory
import System.Environment
import Data.List.Split
import Data.Sort
import Debug.Trace
import Data.Set(Set, unions, intersection, empty, fromList)

countAnswers :: String -> Set Char
countAnswers group =
    let answers = splitOn "\n" group
    in unions $ map fromList answers

countAnswersP2 :: String -> Set Char
countAnswersP2 group =
    let answers = filter (not . null) $ splitOn "\n" group
        res = foldl intersection (fromList ['a'..'z']) $ map fromList answers
    in trace ("Group: " ++ show group ++ " -> " ++ show res) res


main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let groups = splitOn "\n\n" content
  let part1Result = sum $ map (length . countAnswers) groups
  print ("Part 1: " ++ show part1Result)
  let part2Result = sum $ map (length . countAnswersP2) groups
  print ("Part 2: " ++ show part2Result)

  let test1 = countAnswersP2 "xjylztwnkbma\napxzvjlwnbdyk\nzlnbrkyagwjx\nykslxwbjuzang\nwjkbylazxne"
  print ("Test 1: " ++ show test1)
