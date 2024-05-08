module Main where

import AOC
import Control.Monad.ST
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as M
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import qualified Data.Sequence as S

calcNumSteps :: (Int -> Int) -> String -> Integer
calcNumSteps f input = goNext 0 0 nums
    where nums = S.fromList $ map read $ lines input
          goNext c i s
            | i < 0 || i >= S.length s = c
            | otherwise = let val = S.index s i
                          in goNext (c + 1) (i + val) (S.update i (f val) s)

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

type Input = V.Vector Int

prepare :: String -> Input
prepare = V.fromList .  map read . lines

solve :: Input -> (Int -> Int) -> Int -> Int -> Int
solve v f p c
 | p < 0 || p >= V.length v = c
 | otherwise = solve (v V.// [(p, f j)]) f (p + j) (c + 1)
   where j = undefined

solveV i f = runST $ V.thaw i >>= go 0 0
  where go c p v
          | p < 0 || p >= M.length v = return c
          | otherwise = do
                          jmp <- M.read v p
                          M.write v p $ f jmp
                          go (c+1) (p+jmp) v

main :: IO()
main = do
  -- input <- prepare <$> readFile "./input.txt"
  i1 <- readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (calcNumSteps (\s -> if s > 2 then s-1 else s+1) i1)
  -- putStrLn $ "Part 2: " <> show (solveV input (\j -> if j >= 3 then j - 1 else j + 1))
