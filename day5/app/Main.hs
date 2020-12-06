module Main where

--import Text.Parsec
import System.Directory
import System.Environment
import Data.List.Split
import Debug.Trace

data Range = Range Int Int

splitRange :: Range -> Char -> Range
splitRange (Range from to) part =
   let diff = to - from
       half = diff `div` 2
   in case part of
     x | x == 'B' || x == 'R' -> Range (from + half + 1) to
     x | x == 'F' || x == 'L' -> Range from (from + half)

toNumber :: Range -> Int
toNumber (Range a b) =
  a


parseBoardingPass :: String -> (Int, Int)
parseBoardingPass pass =
   let (rowPart, columnPart) = splitAt 7 pass
       row = toNumber $ foldl splitRange (Range 0 127) rowPart
       column = toNumber $ foldl splitRange (Range 0 7) columnPart
   in (row, column)

rowId :: (Int, Int) -> Int
rowId (row, column) =
        row * 8 + column

main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  --print content
  let test1 = "FBFBBFFRLR"
  let test1Res = rowId $ parseBoardingPass test1
  print ("Test 1: " ++ show test1Res)
  let passes = splitOn "\n" content
  let seats = map parseBoardingPass passes
  let largestId = maximum $ map rowId seats
  print ("Result part 1: " ++ show largestId)
