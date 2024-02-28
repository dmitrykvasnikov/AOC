module Main where

type Input = String

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- readFile filepath
  return input

part1 :: Input -> Int
part1 input = 42

part2 :: Input -> Int
part2 input = 42

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part1 input)
