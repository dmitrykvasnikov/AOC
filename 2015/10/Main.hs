module Main where

import Data.List

type Input = String

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- readFile filepath
  return input

sayOnce :: String -> String
sayOnce =  concat . (map (\s -> (show . length) s <> [head s]) ) . group

part1 :: Input -> Int -> Int
part1 input it = length . head . drop it $ iterate sayOnce input

part2 :: Input -> Int
part2 input = 42

main :: IO()
main = do
  let input = "1321131112"
  putStrLn $ "Part 1: " <> show (part1 input 40)
  putStrLn $ "Part 2: " <> show (part1 input 50)
