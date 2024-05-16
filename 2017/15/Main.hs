module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), reFoldl)
import Text.Regex.Applicative.Common (decimal)
import Data.List (foldl')
import Data.Bits

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

type Input = (Int,Int)
input :: Input
input = (65,8921)

toBit :: Int -> Char
toBit 0 = '0'
toBit 1 = '1'
toBit _ = error "no bits"

toBits :: Int -> String
toBits n = go n []
            where go :: Int -> String -> String
                  go n' r
                    | length r == 16 = r
                    | n' == 0        = (replicate (16 - length r) '0') ++ r
                    | otherwise      = go (div n' 2) (toBit (mod n' 2) : r)

next :: Char -> Int -> Int
next 'a' n = (n * 16807) `mod` 2147483647
next 'b' n = (n * 48271) `mod` 2147483647
next _ _   = error "step"

step :: (Int,Int) -> (Int,Int)
step (a,b) = (next 'a' a, next 'b' b)

run1 :: Int -> (Int,Int) -> Int -> Int
run1 rounds gens count
  | rounds == 0 = count
  | otherwise   = run1 (rounds - 1) gens' count'
     where gens' = step gens
           count' = if ((toBits $ fst gens) == (toBits $ snd gens)) then count + 1 else count

part1 :: Input -> Int
part1 input = length . snd $ foldl' go (step input, []) [1..40000000]
  where go :: (Input, [Input]) -> Int -> (Input, [Input])
        go (g@(a,b), res) _ = if toBits a == toBits b then (step g, g:res) else (step g, res)

part2 :: Input -> Int
part2 input = 42

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
