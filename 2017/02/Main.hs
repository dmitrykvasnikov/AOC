module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import System.Posix.Internals (o_BINARY)

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

type Input = [[Int]]

getCheckSum1 :: Input -> Int
getCheckSum1 = sum . map (diff . minmax)
  where diff (mn, mx) = mx - mn
        minmax = foldl (\(x,y) n -> (min x n, max y n)) (maxBound :: Int, minBound :: Int)


getCheckSum2 :: Input -> Int
getCheckSum2 = sum . map (division . pairs)
  where pairs ns = [(x,y) | x <- ns, y <- ns, x /= y]
        division [] = error "No dividers found"
        division ((x,y):ps)
          | x `mod` y == 0 = x `div` y
          | otherwise      = division ps

line :: Parser [Int]
line = sepBy decimal (many (sym '\t'))

prepare :: String -> Input
prepare = catMaybes . map (match line) . lines

part1 :: Input -> Int
part1 = getCheckSum1

part2 :: Input -> Int
part2 = getCheckSum2

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
