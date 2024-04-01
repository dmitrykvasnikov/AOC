module Main where

type Input = String

n :: Int
n = 252533

type Coord = (Int, Int)

coord :: Coord -> Int
coord (y,x)  = go (1,1) 20151125
  where go (y', x') res
          | x' == x && y' == y = res
          | otherwise = go  crd $! (res * n `mod` 33554393)
              where crd = if y' == 1 then (x' + 1, 1) else (y'-1, x' + 1)


part1 :: Int
part1 = coord (2,1)

part2 :: Int
part2 = 42

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1)
  putStrLn $ "Part 2: " <> show (part2)
