module AOC2024D1 (solution, part1, part2) where

import           Control.Arrow ((&&&))

type Input = String

getInput :: IO Input
getInput = pure "Not implemented yet"

part1 :: Input -> String
part1 i = i

part2 :: Input -> String
part2 i = i

solution :: IO (String, String)
solution = getInput >>= pure . (part1 &&& part2)
