module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (digitToInt)
import Data.List (foldl')
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)

-- /** Utilities
type Parser a = RE Char a

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = (:) <$> p <*> many (s *> p)

plus, minus :: forall a . (Enum a, Bounded a) => a -> Int -> a
plus v n = toEnum ((fromEnum v + n) `mod`  (fromEnum (maxBound :: a) + 1))
minus v n = plus v (-n)
inc, dec :: forall a . (Enum a, Bounded a) => a -> a
inc v = plus v 1
dec v = plus v (-1)
-- **/

type Input = [Int]

prepare :: String -> Input
prepare = map digitToInt . head . lines

solve1 :: Input -> Int
solve1 (x:y:rest) = if x == y
                       then x + (solve1 $ y:rest)
                       else solve1 $ y:rest
solve1 _          = 0

solve2 inp = let l = length inp
              in foldl' (\r (i, v) -> if (inp !! i) == inp !! ((i + (l `div` 2)) `mod` l) then r + v else r ) 0 $ zip [0..] inp

part1 :: Input -> Int
part1 input = solve1 $ input ++ [head input]

part2 :: Input -> Int
part2 = solve2

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
