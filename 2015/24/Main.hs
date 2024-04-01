module Main where
import Data.List (sortOn)

type Input = [Int]

sets :: [Int] -> Int -> [[Int]]
sets xs n = go xs (sum xs `div` n) (length xs `div` n)  where
  go ys q l
    | q == 0 = [[]]
    | q <0   = []
    | l == 0 = []
    | ys == [] = []
    | otherwise = let (y:ys') = ys
                  in (map (y:) (go ys' (q - y) (l - 1))) ++ go ys' q l

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- map read . lines <$> readFile filepath
  return input

skip :: [[Int]] -> [[Int]]
skip xs = let xs' = sortOn length xs
              l = length $ head xs'
          in takeWhile (\x -> length x == l) xs'

part1 :: Input -> Int
part1 xs = minimum. map product . skip. sets xs $ 3
part2 :: Input -> Int
part2 xs = minimum. map product . skip . sets xs $ 4

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
