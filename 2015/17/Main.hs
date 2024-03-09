module Main where

import Data.List (group, sort)

type Input = [Int]

func :: Int -> [Int] -> Int
func 0 _      = 1
func _ []     = 0
func r (b:bs) = func (r - b) bs + func r bs

func2 :: Int -> [Int] -> [[Int]]
func2 r []
  | r == 0 = [[]]
  | otherwise = []
func2 r (b:bs)
  | r == 0 = [[]]
  | r < 0 = []
  | otherwise = (map (b:) (func2 (r - b) bs)) ++ func2 r bs

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- map read <$> lines <$> readFile filepath
  return input

part1 :: Input -> Int
part1 = func 150

part2 :: Input -> Int
part2 = length . head . group . sort . map length . func2 150

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
