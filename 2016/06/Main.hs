module Main where

import Data.List (sort, sortOn, transpose, group)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Type.Coercion (trans)

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

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- lines <$> readFile filepath
  return input

part1 :: Input -> String
part1 = map head . map (head . reverse . sortOn length . group . sort) . transpose

part2 :: Input -> String
part2 = map head . map (head . sortOn length . group . sort) . transpose

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
