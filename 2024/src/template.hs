module AOC2024D1 (solution, part1, part2) where

import           Control.Arrow ((&&&))

type Input = String

getInput :: IO Input
getInput = do
  i <- readFile "./input/day"
  pure "Not implemented yet"

part1 :: Input -> Int
part1 i = 0

part2 :: Input -> Int
part2 i = 0

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
