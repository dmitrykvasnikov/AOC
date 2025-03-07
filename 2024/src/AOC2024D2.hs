module AOC2024D2 (solution, part1, part2) where

import           Control.Arrow   ((&&&))
import           Data.List.Split (splitOn)

type Level = Int

type Report = [Level]

type Input = [Report]

data Order = DEC | INC | EQL deriving (Eq)

getInput :: IO Input
getInput = readFile "./input/day2" >>= pure . map (map read . splitOn " ") . lines

getOrder :: Int -> Int -> Order
getOrder x y
  | x == y = EQL
  | x < y = INC
  | otherwise = DEC

isValidRecord :: Report -> Bool
isValidRecord [] = True
isValidRecord [x] = True
isValidRecord r@(x : y : _)
  | x == y = False
  | otherwise = go (getOrder x y) r
  where
    go :: Order -> Report -> Bool
    go _ [] = True
    go _ [x] = True
    go o (x : y : rest) =
      if (o == getOrder x y) && (abs (x - y) >= 1) && (abs (x - y) <= 3) then go o (y : rest) else False

part1 :: Input -> Int
part1 = length . filter isValidRecord

part2 :: Input -> Int
part2 i = 0

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
