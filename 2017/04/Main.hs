module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (nub, permutations, sort)
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

type Input = [String]

solve1, solve2 :: Input -> Int
solve1 = length . filter (\s -> length s == (length . nub) s) . map words
solve2 = length . filter (\s -> length s == (length . nub) s) . map (map sort . words)

prepare :: String -> Input
prepare = lines

part1 :: Input -> Int
part1 = solve1

part2 :: Input -> Int
part2 = solve2

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
