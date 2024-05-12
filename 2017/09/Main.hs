module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), reFoldl)
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

data Mode = Normal | Garbage | Ignore deriving (Show, Eq)

filter1 :: String -> Mode -> String
filter1 [] _ = []
filter1 (x:xs) Normal
  | x == '<' = filter1 xs Garbage
  | elem x ("{}" :: String) = x : (filter1 xs Normal)
  | otherwise = filter1 xs Normal
filter1 (x:xs) Garbage
  | x == '>' = filter1 xs Normal
  | x == '!' = filter1 xs Ignore
  | otherwise = filter1 xs Garbage
filter1 (_:xs) Ignore = filter1 xs Garbage

filter2 :: String -> Mode -> String
filter2 [] _ = []
filter2 (x:xs) Normal
  | x == '<' = filter2 xs Garbage
  | otherwise = filter2 xs Normal
filter2 (x:xs) Garbage
  | x == '>' = filter2 xs Normal
  | x == '!' = filter2 xs Ignore
  | otherwise = x : (filter2 xs Garbage)
filter2 (_:xs) Ignore = filter2 xs Garbage

count :: String -> Int -> Int -> Int
count [] d r = r
count (x:xs) d r
 | x == '{' = count xs (d+1) r
 | x == '}' = count xs (d-1) (r+d)

type Input = String

prepare :: String -> Input
prepare = head . lines

part1 :: Input -> Int
part1 i = count (filter1 i Normal) 0 0

part2 :: Input -> Int
part2 = length . flip filter2 Normal

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
