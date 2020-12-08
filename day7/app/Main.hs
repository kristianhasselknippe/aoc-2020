module Main where

import System.Directory
import System.Environment
import Text.Parsec
import Flow
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec.Prim(Parser)

parseBag = do
  modifier <- many1 letter
  space
  color <- many1 letter
  space
  choice [try $ string "bags", try $ string "bag"]
  return (modifier, color)

type BagInfo = (String, String)
type InnerBag = (Int, BagInfo)
type Bag = (BagInfo, [InnerBag])

parseInnerBag :: Parser InnerBag
parseInnerBag = do
  amount <- read <$> many1 digit
  space
  bagInfo <- parseBag
  return (amount, bagInfo)

parseLine :: Parser Bag
parseLine = do
  bag <- parseBag
  space
  string "contain"
  space
  innerBags <- (string "no other bags" >> return []) <|> sepBy parseInnerBag (string ", ")
  string "."
  return (bag, innerBags)

parseLines = many (parseLine <* many1 space)

merge :: [Bool] -> Bool
merge [h,t] = h || t
merge (x:xs) = x || merge xs
merge [] = False

type BagMap = Map.Map BagInfo Bag

canHaveBag :: BagInfo -> BagMap -> Bag -> Bool
canHaveBag target bags bag =
  let (_, innerBags) = bag
      innerBagInfos = map snd innerBags
      thisHasTarget = target `elem` innerBagInfos
      innerHasTarget = merge $ map (\innerBagInfo -> canHaveBag target bags (bags Map.! innerBagInfo)) innerBagInfos
  in thisHasTarget || innerHasTarget

howManyBagsContained :: BagMap -> Bag -> Int
howManyBagsContained bagMap (info, innerBags) =
  let numBagsInThis = innerBags |> map fst |> sum
      childBags = innerBags |> map (\(count, bagInfo) -> bagMap Map.! bagInfo |> howManyBagsContained bagMap |> (*) count) |> sum
  in numBagsInThis + childBags


main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let (Right parsed) = content |> parse parseLines ""
  let bags = Map.fromList (parsed |> map (\x -> (fst x, x)))
  let target = ("shiny", "gold")
  let res = length $ filter id $ map (canHaveBag target bags) parsed
  print ("Part 1: " ++ show res)
  let resPart2 = howManyBagsContained bags (bags Map.! target)
  print ("Part 2: " ++ show resPart2)
