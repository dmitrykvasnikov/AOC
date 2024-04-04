module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sort, transpose)
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

type Triangle = [Int]

type Input = [Triangle]

triangle :: Parser Triangle
triangle = some (many (sym ' ') *> decimal)

convert :: [Triangle] -> [Triangle]
convert [] = []
convert (x:y:z:rest) = transpose (x:y:z:[]) ++ convert rest

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- catMaybes . map  (match triangle) . lines <$> readFile filepath
  return input

part1 :: Input -> Int
part1 = length . filter isTriangle . map sort
  where isTriangle (x:y:z:_) = (x + y) > z

part2 :: Input -> Int
part2 = part1 . convert

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
  putStrLn "Done"
